{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Mutagen.Test.Loop where

import Control.Monad
import Control.Monad.Extra (ifM)
import Data.Function ((&))
import Data.Maybe
import qualified Data.PQueue.Prio.Min as PQueue
import System.Random (split)
import Test.Mutagen.Fragment
import Test.Mutagen.Lazy
import Test.Mutagen.Mutation
import Test.Mutagen.Property
import Test.Mutagen.Test.Batch
import Test.Mutagen.Test.Report
import Test.Mutagen.Test.State
import Test.Mutagen.Test.Terminal
import Test.Mutagen.Tracer.Store (STraceType (..), TraceStoreImpl (..))
import Test.Mutagen.Tracer.Trace (Trace (..), TraceNode, withTrace)
import Test.QuickCheck.Gen (unGen)

----------------------------------------
-- The main test loop

loop :: TestCaseRunner -> MutagenState -> IO Report
loop runner st
  -- We reached the max number of tests
  | stNumPassed st >= stMaxSuccess st =
      if stExpect st
        then success st
        else noExpectedFailure st
  -- We discarded too many tests
  | stNumDiscarded st >= stMaxDiscardRatio st * max (stNumPassed st) (stMaxSuccess st) =
      giveUp st "too many discarded tests"
  -- The time bugdet is over, we check this every so often
  | (stNumPassed st + stNumDiscarded st) `mod` 100 == 0 =
      ifM
        (passedTimeout st)
        (giveUp st "timeout")
        (newTest runner st)
  -- Reset the trace log if we havent enqueued anything interesting in a while
  -- additionally, increase the number of random mutations
  | maybe False (stNumTestsSinceLastInteresting st >=) (stAutoResetAfter st)
      && null (stPassedQueue st)
      && null (stDiscardedQueue st) = do
      withPassedTraceStore st resetTraceStore
      withDiscardedTraceStore st resetTraceStore
      let st' =
            st
              & setAutoResetAfter (maybe (stAutoResetAfter st) (Just . (* 2)) (stAutoResetAfter st))
              & setRandomMutations (stRandomMutations st * 2)
              & increaseNumTraceLogResets
              & resetNumTestsSinceLastInteresting
      newTest runner st'
  -- Nothing new under the sun, continue testing
  | otherwise =
      newTest runner st

-- Testing is no more
success :: MutagenState -> IO Report
success st = do
  reportDoneTesting st
  return
    Success
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- Too many discarded tests
giveUp :: MutagenState -> String -> IO Report
giveUp st r = do
  reportGaveUp st r
  return
    GaveUp
      { why = r
      , numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- Found a bug!
counterexample :: MutagenState -> Args -> Test -> IO Report
counterexample st as test = do
  reportCounterexample st as test
  return
    Counterexample
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      , failingArgs = as
      }

noExpectedFailure :: MutagenState -> IO Report
noExpectedFailure st = do
  reportNoExpectedFailure st
  return
    NoExpectedFailure
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- Generate and run a new test
newTest :: TestCaseRunner -> MutagenState -> IO Report
newTest runner st = do
  -- print global stats
  when (stChatty st) $ do
    clear >> printGlobalStats st
  -- pick a new test case
  (args, parent, st') <- pickNextTestCase st
  -- run the test case
  (res, st'') <- runner parent args st'
  -- when we are in deep debug mode stop until the user presses enter
  when (stDebug st'') (putStrLn "Press Enter to continue..." >> void getLine)
  -- check the test result and report a counterexample or continue
  case res of
    -- Test failed and it was expected to, report counterexample
    Just test | stExpect st'' -> counterexample st'' args test
    -- Test failed but it was not expected to, stop testing
    Just _ -> success st''
    -- Test passed or discarded, continue the loop
    Nothing -> loop runner st''

----------------------------------------
-- Test case runners

-- | Test case runners as state 'MutagenState' transition functions
type TestCaseRunner = Maybe (MutationBatch Args) -> Args -> MutagenState -> IO (Maybe Test, MutagenState)

-- | Run the test and check the result, updating the state as necessary.
runTestCase :: TestCaseRunner
runTestCase parent args st = do
  when (stChatty st) $ do
    printRunningTest
  -- Run the test case
  (test, entries, eval_pos) <- execArgsRunner st args

  when (stChatty st && stUseLazyPrunning st) $ do
    printEvaluatedSubexpressions (fromJust eval_pos)
  -- Truncate the trace if it is too long
  let tr = Trace (maybe entries (flip take entries) (stMaxTraceLength st))
  when (stChatty st) $ do
    printMutatedTestCaseTrace tr
  -- Extract property modifiers
  let addModifiers = \s -> s{stExpect = False}
  -- Inspect the test result
  case test of
    -- Boom!
    Failed -> do
      when (stChatty st) $ do
        printTestResult "FAILED"
      let st'' = addModifiers st
      return (Just test, st'')
    -- Test passed, lotta work to do now
    Passed -> do
      when (stChatty st) $ do
        printTestResult "PASSED"
      -- Save the trace in the corresponding trace log
      (new, prio) <- savePassedTraceWithPrio st tr
      -- Evaluate whether the test case was interesting or not
      let interesting = new > 0
      when (stChatty st && interesting) $ do
        printTestCaseWasInteresting new prio
      -- Update the internal state for the next iteration
      let st'
            | interesting = updateStateAfterInterestingPassed st args parent tr eval_pos prio
            | otherwise = updateStateAfterBoringPassed st
          st'' = addModifiers st'
      return (Nothing, st'')
    -- Test discarded, lotta work to do here too
    Discarded -> do
      when (stChatty st) $ do
        printTestResult "DISCARDED"
      -- Save the trace in the corresponding trace log
      (new, prio) <- saveDiscardedTraceWithPrio st tr
      -- Evaluate whether the test case was interesting or not
      let interesting = new > 0 && maybe False mb_test_passed parent
      when (stChatty st && interesting) $ do
        printTestCaseWasInteresting new prio
      -- Update the internal state for the next iteration
      let st'
            | interesting = updateStateAfterInterestingDiscarded st args parent tr eval_pos prio
            | otherwise = updateStateAfterBoringDiscarded st
          st'' = addModifiers st'
      return (Nothing, st'')

savePassedTraceWithPrio :: MutagenState -> Trace -> IO (Int, Int)
savePassedTraceWithPrio st tr =
  case st of
    MutagenState{stTraceType = SBitmap, stPassedTraceLog = store} -> do
      new <- saveTrace tr store
      let prio = stGeneratedTraceNodes st - new
      return (new, prio)
    MutagenState{stTraceType = STree, stPassedTraceLog = store} -> do
      (new, depth) <- saveTrace tr store
      let prio = depth
      return (new, prio)

saveDiscardedTraceWithPrio :: MutagenState -> Trace -> IO (Int, Int)
saveDiscardedTraceWithPrio st tr =
  case st of
    MutagenState{stTraceType = SBitmap, stDiscardedTraceLog = store} -> do
      new <- saveTrace tr store
      let prio = stGeneratedTraceNodes st - new
      return (new, prio)
    MutagenState{stTraceType = STree, stDiscardedTraceLog = store} -> do
      (new, depth) <- saveTrace tr store
      let prio = depth
      return (new, prio)

withPassedTraceStore
  :: MutagenState
  -> (forall trace. (TraceStoreImpl trace) => TraceStore trace -> r)
  -> r
withPassedTraceStore st k =
  case st of MutagenState{stPassedTraceLog = store} -> k store

withDiscardedTraceStore
  :: MutagenState
  -> (forall trace. (TraceStoreImpl trace) => TraceStore trace -> r)
  -> r
withDiscardedTraceStore st k =
  case st of MutagenState{stDiscardedTraceLog = store} -> k store

----------------------------------------

-- Run the test and capture:
--   * The test result (passed, discarded, failed)
--   * The trace in the program it traversed
--   * The the positions of the evaluated expressions of the input

execArgsRunner :: MutagenState -> Args -> IO (Test, [TraceNode], Maybe [Pos])
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

updateStateAfterInterestingPassed :: MutagenState -> Args -> Maybe (MutationBatch Args) -> Trace -> Maybe [Pos] -> Int -> MutagenState
updateStateAfterInterestingPassed st args parent tr eval_pos prio =
  let mbatch = createOrInheritMutationBatch st args parent eval_pos True
   in st
        & increaseNumPassed
        & increaseNumInteresting
        & resetNumTestsSinceLastInteresting
        & setPassedQueue (PQueue.insert prio (args, tr, mbatch) (stPassedQueue st))
        & setFragmentStore
          ( if stUseFragments st
              then storeFragments (stFilterFragments st) args (stFragmentStore st)
              else stFragmentStore st
          )

updateStateAfterInterestingDiscarded :: MutagenState -> Args -> Maybe (MutationBatch Args) -> Trace -> Maybe [Pos] -> Int -> MutagenState
updateStateAfterInterestingDiscarded st args parent tr eval_pos prio =
  let mbatch = createOrInheritMutationBatch st args parent eval_pos False
   in st
        & increaseNumDiscarded
        & increaseNumInteresting
        & resetNumTestsSinceLastInteresting
        & setDiscardedQueue (PQueue.insert prio (args, tr, mbatch) (stDiscardedQueue st))
        & setFragmentStore
          ( if stUseFragments st
              then storeFragments (stFilterFragments st) args (stFragmentStore st)
              else stFragmentStore st
          )

updateStateAfterBoringPassed :: MutagenState -> MutagenState
updateStateAfterBoringPassed st =
  st
    & increaseNumPassed
    & increaseNumBoring
    & increaseNumTestsSinceLastInteresting

updateStateAfterBoringDiscarded :: MutagenState -> MutagenState
updateStateAfterBoringDiscarded st =
  st
    & increaseNumDiscarded
    & increaseNumBoring
    & increaseNumTestsSinceLastInteresting

----------------------------------------
-- Selecting the next test case

-- Select a new test, mutating and existing interesting one or generating a
-- brand new otherwise.

pickNextTestCase :: MutagenState -> IO (Args, Maybe (MutationBatch Args), MutagenState)
pickNextTestCase st
  -- We can run a mutation of an interesting succesful test case
  | not (null (stPassedQueue st)) = mutateFromPassed st
  -- We can run a mutation of an interesting discarded test case
  | not (null (stDiscardedQueue st)) = mutateFromDiscarded st
  -- Only choice left is to generate a brand new test
  | otherwise = generateNewTest st

generateNewTest :: MutagenState -> IO (Args, Maybe (MutationBatch Args), MutagenState)
generateNewTest st = do
  -- First we compute an appropriate generation size
  let size = computeSize st
  -- Then we randomly generate a lazily evaluated test
  let (rnd1, rnd2) = split (stNextSeed st)
  let args = unGen (stArgsGen st) rnd1 size
  if stChatty st then printGeneratedTestCase args else printDot
  -- Put the new random seed in the state
  let st' =
        st
          & setNextSeed rnd2
          & setCurrentGenSize size
          & increaseNumGenerated
  return (args, Nothing, st')

mutateFromPassed :: MutagenState -> IO (Args, Maybe (MutationBatch Args), MutagenState)
mutateFromPassed st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stPassedQueue st)
  next <- nextMutation (stFragmentStore st) mbatch
  case next of
    Nothing -> do
      let st' = st & setPassedQueue rest
      pickNextTestCase st'
    Just (args', mk', mbatch') -> do
      when (stChatty st) $ do
        printOriginalTestCase prio args True
        printBatchStatus mbatch
        printOriginalTestCaseTrace tr
      if stChatty st then printMutatedTestCase args' else printDot
      let st' =
            st
              & increaseMutantKindCounter mk'
              & increaseNumMutatedFromPassed
              & setPassedQueue (PQueue.insert prio (args, tr, mbatch') rest)
      return (args', Just mbatch', st')

mutateFromDiscarded :: MutagenState -> IO (Args, Maybe (MutationBatch Args), MutagenState)
mutateFromDiscarded st = do
  let ((prio, (args, tr, mbatch)), rest) = PQueue.deleteFindMin (stDiscardedQueue st)
  next <- nextMutation (stFragmentStore st) mbatch
  case next of
    Nothing -> do
      let st' = st & setDiscardedQueue rest
      pickNextTestCase st'
    Just (args', mk', mbatch') -> do
      when (stChatty st) $ do
        printOriginalTestCase prio args False
        printBatchStatus mbatch
        printOriginalTestCaseTrace tr
      if stChatty st then printMutatedTestCase args' else printDot
      let st' =
            st
              & increaseMutantKindCounter mk'
              & increaseNumMutatedFromDiscarded
              & setDiscardedQueue (PQueue.insert prio (args, tr, mbatch') rest)
      return (args', Just mbatch', st')
