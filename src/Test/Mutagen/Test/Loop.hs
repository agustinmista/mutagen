module Test.Mutagen.Test.Loop where

import Control.Monad
import Control.Monad.Extra (ifM)
import Text.Printf

import Data.Maybe

import qualified Data.PQueue.Prio.Min as PQueue

import System.IO
import System.Random (split)

import Test.QuickCheck.Gen (unGen)

import Test.Mutagen.Tracer
import Test.Mutagen.Property
import Test.Mutagen.Lazy
import Test.Mutagen.Mutation
import Test.Mutagen.Fragment
import Test.Mutagen.Test.State
import Test.Mutagen.Test.Batch
import Test.Mutagen.Test.Report
import Test.Mutagen.Test.Terminal

----------------------------------------
-- The main test loop

-- We abstract the test case runner based on the way it logs execution traces
type TestCaseRunner log = State log -> Maybe (MutationBatch Args) -> Args -> IO (Either Test (State log))

loop :: TraceLogger log => TestCaseRunner log -> State log -> IO Report
loop runner st
  -- We reached the max number of tests
  | stNumPassed st >= stMaxSuccess st =
      doneTesting st
  -- We discarded too many tests
  | stNumDiscarded st >= stMaxDiscardRatio st * max (stNumPassed st) (stMaxSuccess st) =
      giveUp st "too many discarded tests"
  -- The time bugdet is over, we check this every so often
  | (stNumPassed st + stNumDiscarded st) `mod` 100 == 0 =
      ifM (passedTimeout st)
          (giveUp st "timeout")
          (newTest runner st)
  -- Reset the trace log if we havent enqueued anything interesting in a while
  -- additionally, increase the number of random mutations
  | maybe False (stNumTestsSinceLastInteresting st >=) (stAutoResetAfter st) &&
    null (stPassedQueue st) &&
    null (stDiscardedQueue st) = do
      resetTraceLog (stPassedTraceLog st)
      resetTraceLog (stDiscardedTraceLog st)
      let st' = st ! setAutoResetAfter (maybe (stAutoResetAfter st) (Just . (*2)) (stAutoResetAfter st))
                   ! setRandomMutations (stRandomMutations st * 2)
                   ! increaseNumTraceLogResets
                   ! resetNumTestsSinceLastInteresting
      newTest runner st'
  -- Nothing new under the sun, continue testing
  | otherwise =
      newTest runner st

-- Testing is no more
doneTesting :: TraceLogger log => State log -> IO Report
doneTesting st = do
  clear
  printf "Done testing\n"
  reportFinalStats st
  return AllPassed
    { numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    }

-- Too many discarded tests
giveUp :: TraceLogger log => State log -> String -> IO Report
giveUp st r = do
  clear
  printf "Gave up (%s)\n" r
  reportFinalStats st
  return GaveUp
    { why = r
    , numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    }

-- Found a bug!
counterexample :: TraceLogger log => State log -> Args -> Test -> IO Report
counterexample st as test = do
  clear
  reportCounterexample as test
  reportFinalStats st
  return Counterexample
    { numPassed = stNumPassed st
    , numDiscarded = stNumDiscarded st
    , failingArgs = as
    }

-- Generate and run a new test
newTest :: TraceLogger log => TestCaseRunner log -> State log -> IO Report
newTest runner st = do
  -- when we are in deep debug mode stop until the user presses enter
  when (stStepByStep st) (void getLine)
  -- print the header
  clear >> hFlush stdout
  printGlobalStats st
  -- pick a new test case
  (args, parent, st') <- pickNextTestCase st
  -- run the test case
  runRes <- runner st' parent args
  -- check the test result and continue or report a counterexample
  either (counterexample st' args) (loop runner) runRes

----------------------------------------
-- Test case runners

-- Run the test and check the result, if it passes then continue testing
runTestCase_tree :: TestCaseRunner TraceTreeLog
runTestCase_tree st parent args = do
  when (stChatty st) $ do
    printf "\nRunning test...\n"
  -- Run the test case
  (test, entries, eval_pos) <- execArgsRunner st args
  when (stChatty st && stUseLazyPrunning st) $ do
    printf "\nEvaluated subexpressions:\n%s\n" (show (fromJust eval_pos))
  -- Truncate the trace if it is too long
  let tr = Trace (maybe entries (flip take entries) (stMaxTraceLength st))
  when (stChatty st) $ do
    printMutatedTestCaseTrace tr
  -- Inspect the test result
  case test of
    -- Boom!
    Failed -> do
      when (stChatty st) $ do
        printf "\nTest result: FAILED\n"
      return (Left test)
    -- Test passed, lotta work to do now
    Passed -> do
      when (stChatty st) $ do
        printf "\nTest result: PASSED\n"
      -- Save the trace in the corresponding trace log
      (new, depth) <- registerTrace tr (stPassedTraceLog st)
      -- Evaluate whether the test case was interesting or not
      let interesting = new > 0
      let prio = depth
      when (stChatty st && interesting) $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new prio
      -- Update the internal state for the next iteration
      let st' | interesting = updateStateAfterInterestingPassed st args parent tr eval_pos prio
              | otherwise   = updateStateAfterBoringPassed st
      return (Right st')
    -- Test discarded, lotta work to do here too
    Discarded -> do
      when (stChatty st) $ do
        printf "\nTest result: DISCARDED\n"
      -- Save the trace in the corresponding trace log
      (new, depth) <- registerTrace tr (stDiscardedTraceLog st)
      -- Evaluate whether the test case was interesting or not
      let interesting = new > 0 && maybe False mb_test_passed parent
      let prio = depth
      when interesting $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new prio
      -- Update the internal state for the next iteration
      let st' | interesting = updateStateAfterInterestingDiscarded st args parent tr eval_pos prio
              | otherwise   = updateStateAfterBoringDiscarded st
      return (Right st')


-- Run the test and check the result, if it passes then continue testing
runTestCase_bitmap :: TestCaseRunner TraceBitmapLog
runTestCase_bitmap st parent args = do
  when (stChatty st) $ do
    printf "\nRunning test...\n"
  -- Run the test case
  (test, entries, eval_pos) <- execArgsRunner st args
  when (stChatty st && stUseLazyPrunning st) $ do
    printf "\nEvaluated subexpressions:\n%s\n" (show (fromJust eval_pos))
  -- Truncate the trace if it is too long
  let tr = Trace (maybe entries (flip take entries) (stMaxTraceLength st))
  when (stChatty st) $ do
    printMutatedTestCaseTrace tr
  -- Inspect the test result
  case test of
    -- Boom!
    Failed -> do
      when (stChatty st) $ do
        printf "\nTest result: FAILED\n"
      return (Left test)
    -- Test passed, lotta work to do now
    Passed -> do
      when (stChatty st) $ do
        printf "\nTest result: PASSED\n"
      -- Save the trace in the corresponding trace log
      new <- registerTrace tr (stPassedTraceLog st)
      -- Evaluate whether the test case was interesting or not
      let interesting = new > 0
      let prio = stGeneratedTraceNodes st - new
      when (stChatty st && interesting) $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new prio
      -- Update the internal state for the next iteration
      let st' | interesting = updateStateAfterInterestingPassed st args parent tr eval_pos prio
              | otherwise   = updateStateAfterBoringPassed st
      return (Right st')
    -- Test discarded, lotta work to do here too
    Discarded -> do
      when (stChatty st) $ do
        printf "\nTest result: DISCARDED\n"
      -- Save the trace in the corresponding trace log
      new <- registerTrace tr (stDiscardedTraceLog st)
      -- Evaluate whether the test case was interesting or not
      let interesting = new > 0 && maybe False mb_test_passed parent
      let prio = stGeneratedTraceNodes st - new
      when interesting $ do
        printf "\nTest case was interesting! (new trace nodes=%d, prio=%d)\n" new prio
      -- Update the internal state for the next iteration
      let st' | interesting = updateStateAfterInterestingDiscarded st args parent tr eval_pos prio
              | otherwise   = updateStateAfterBoringDiscarded st
      return (Right st')

----------------------------------------

-- Run the test and capture:
--   * The test result (passed, discarded, failed)
--   * The trace in the program it traversed
--   * The the positions of the evaluated expressions of the input

execArgsRunner :: State log -> Args -> IO (Test, [TraceEntry], Maybe [Pos])
execArgsRunner st args
  | stUseLazyPrunning st = do
      resetPosRef
      (test, Trace entries) <- withTrace (unResult (protectResult (stArgsRunner st (lazy args))))
      evaluated <- readPosRef
      return (test, entries, Just evaluated)
  | otherwise = do
      (test, Trace entries) <- withTrace (unResult (protectResult (stArgsRunner st args)))
      return (test, entries, Nothing)

----------------------------------------

-- Update the internal state for the next iteration depending on whether the
-- test passed or was discarded and also if it was interesting.

updateStateAfterInterestingPassed :: State log -> Args -> Maybe (MutationBatch Args) -> Trace -> Maybe [Pos] -> Int -> State log
updateStateAfterInterestingPassed st args parent tr eval_pos prio =
  let mbatch = createOrInheritMutationBatch st args parent eval_pos True in
  st ! increaseNumPassed
     ! increaseNumInteresting
     ! resetNumTestsSinceLastInteresting
     ! setPassedQueue (PQueue.insert prio (args, tr, mbatch) (stPassedQueue st))
     ! setFragmentStore (if stUseFragments st then storeFragments args (stFragmentStore st) else stFragmentStore st)

updateStateAfterInterestingDiscarded :: State log -> Args -> Maybe (MutationBatch Args) -> Trace -> Maybe [Pos] -> Int -> State log
updateStateAfterInterestingDiscarded st args parent tr eval_pos prio =
  let mbatch = createOrInheritMutationBatch st args parent eval_pos False in
  st ! increaseNumDiscarded
     ! increaseNumInteresting
     ! resetNumTestsSinceLastInteresting
     ! setDiscardedQueue (PQueue.insert prio (args, tr, mbatch) (stDiscardedQueue st))
     ! setFragmentStore (if stUseFragments st then storeFragments args (stFragmentStore st) else stFragmentStore st)

updateStateAfterBoringPassed :: State log -> State log
updateStateAfterBoringPassed st =
  st ! increaseNumPassed
     ! increaseNumBoring
     ! increaseNumTestsSinceLastInteresting

updateStateAfterBoringDiscarded :: State log -> State log
updateStateAfterBoringDiscarded st =
  st ! increaseNumDiscarded
     ! increaseNumBoring
     ! increaseNumTestsSinceLastInteresting

----------------------------------------
-- Selecting the next test case

-- Select a new test, mutating and existing interesting one or generating a
-- brand new otherwise.

pickNextTestCase :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
pickNextTestCase st
  -- We can run a mutation of an interesting succesful test case
  | not (null (stPassedQueue st)) = mutateFromPassed st
  -- We can run a mutation of an interesting discarded test case
  | not (null (stDiscardedQueue st)) = mutateFromDiscarded st
  -- Only choice left is to generate a brand new test
  | otherwise = generateNewTest st

generateNewTest :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
generateNewTest st = do
  -- First we compute an appropriate generation size
  let size = computeSize st
  -- Then we randomly generate a lazily evaluated test
  let (rnd1, rnd2) = split (stNextSeed st)
  let args = unGen (stArgsGen st) rnd1 size
  printGeneratedTestCase args
  -- Put the new random seed in the state
  let st' = st ! setNextSeed rnd2
               ! setCurrentGenSize size
               ! increaseNumGenerated
  return (args, Nothing, st')

mutateFromPassed :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
mutateFromPassed st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stPassedQueue st)
  next <- nextMutation (stFragmentStore st) mbatch
  case next of
    Nothing -> do
      let st' = st ! setPassedQueue rest
      pickNextTestCase st'
    Just (args', mk', mbatch') -> do
      when (stChatty st) $ do
        printOriginalTestCase prio args True
        printBatchStatus mbatch
        printOriginalTestCaseTrace tr
      printMutatedTestCase args'
      let st' = st ! increaseMutantKindCounter mk'
                   ! increaseNumMutatedFromPassed
                   ! setPassedQueue (PQueue.insert prio (args, tr, mbatch') rest)
      return (args', Just mbatch', st')

mutateFromDiscarded :: TraceLogger log => State log -> IO (Args, Maybe (MutationBatch Args), State log)
mutateFromDiscarded st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stDiscardedQueue st)
  next <- nextMutation (stFragmentStore st) mbatch
  case next of
    Nothing -> do
      let st' = st ! setDiscardedQueue rest
      pickNextTestCase st'
    Just (args', mk', mbatch') -> do
      when (stChatty st) $ do
        printOriginalTestCase prio args False
        printBatchStatus mbatch
        printOriginalTestCaseTrace tr
      printMutatedTestCase args'
      let st' = st ! increaseMutantKindCounter mk'
                   ! increaseNumMutatedFromDiscarded
                   ! setDiscardedQueue (PQueue.insert prio (args, tr, mbatch') rest)
      return (args', Just mbatch', st')
