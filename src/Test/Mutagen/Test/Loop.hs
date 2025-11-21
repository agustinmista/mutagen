{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Mutagen's main testing loop
module Test.Mutagen.Test.Loop
  ( -- * Test loop
    loop
  )
where

import Control.Monad
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import System.Random (split)
import Test.Mutagen.Config (DebugMode (..))
import Test.Mutagen.Fragment
import Test.Mutagen.Lazy
import Test.Mutagen.Mutation
import Test.Mutagen.Property
import Test.Mutagen.Report
import Test.Mutagen.Test.Queue
  ( MutationBatch (..)
  , MutationCandidate (..)
  , createOrInheritMutationBatch
  , dequeueNextMutationCandidate
  , enqueueMutationCandidate
  , mutationQueueSize
  , nextMutation
  )
import Test.Mutagen.Test.State
import Test.Mutagen.Test.Terminal
import Test.Mutagen.Tracer.Store (STraceType (..), TraceStoreImpl (..))
import Test.Mutagen.Tracer.Trace (Trace (..), truncateTrace, withTrace)
import Test.QuickCheck.Gen (unGen)

{-------------------------------------------------------------------------------
-- * The main test loop
-------------------------------------------------------------------------------}

-- | Constraint alias for monads that can run Mutagen tests
type MonadMutagen m = (MonadIO m, MonadTerminal m)

-- | The entry point to Mutagen's main testing loop
--
-- The state machine switches back and forth between 'loop' and 'newTest',
-- until one of the terminal conditions is met.
loop :: (MonadMutagen m) => MutagenState -> m Report
loop st
  -- We reached the max number of tests
  -- ==> either success or expected failure did not occur
  | stNumPassed st >= stMaxSuccess st =
      if stExpect st
        then success st
        else noExpectedFailure st
  -- We discarded too many tests
  -- ==> give up
  | stNumDiscarded st
      >= stMaxDiscardRatio st * max (stNumPassed st) (stMaxSuccess st) =
      giveUp st "too many discarded tests"
  -- The time bugdet is over, we check this every so often
  -- ==> give up
  | (stNumPassed st + stNumDiscarded st) `mod` 100 == 0 =
      ifM
        (liftIO (timedOut st))
        (giveUp st "timeout")
        (newTest st)
  -- There has been a long time since we enqueued anything interesting and
  -- both mutation queues are empty
  -- ==> reset the trace logs to free memory
  -- ==> double the number of random mutations
  -- ==> double the auto-reset threshold
  -- ==> continue testing
  | Just threshold <- stAutoResetAfter st
  , stNumTestsSinceLastInteresting st >= threshold
  , mutationQueueSize (stPassedQueue st) == 0
  , mutationQueueSize (stDiscardedQueue st) == 0 = do
      withPassedTraceStore st (liftIO . resetTraceStore)
      withDiscardedTraceStore st (liftIO . resetTraceStore)
      let st' =
            st
              & setAutoResetAfter ((* 2) <$> (stAutoResetAfter st))
              & setRandomMutations (stRandomMutations st * 2)
              & incNumTraceLogResets
              & resetNumTestsSinceLastInteresting
      newTest st'
  -- Nothing new under the sun
  -- ==> continue testing
  | otherwise =
      newTest st

-- | Select the next test case, run it, and process the result
newTest :: (MonadMutagen m) => MutagenState -> m Report
newTest st = do
  -- pick a new test case
  (args, parent, st') <- pickNextTestCase st
  -- run the test case
  (result, st'') <- runTestCase args parent st'
  -- Print stats if necessary
  if stChatty st'' then printGlobalStats st'' else printShortStats st''
  -- Stop on debug mode if necessary
  stopOnDebugMode (stDebug st'') result
  -- check the test result and report a counterexample or continue
  case result of
    -- Test failed and it was not supposed to, report counterexample
    Failed | stExpect st'' -> counterexample st'' args result
    -- Test failed but it was expected to, stop testing
    Failed -> success st''
    -- Test passed or discarded, continue the loop
    _ -> loop st''
  where
    -- Stop execution if in debug mode
    stopOnDebugMode debugMode res =
      case debugMode of
        StopOnPassed | Passed <- res -> awaitForUserInput
        AlwaysStop -> awaitForUserInput
        _ -> return ()
    awaitForUserInput = do
      message "Press enter to continue ..."
      void (liftIO getLine)

-- * Terminal states

-- | All tests passed successfully
success :: (MonadMutagen m) => MutagenState -> m Report
success st = do
  message "Done testing"
  return
    Success
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- | Too many discarded tests
giveUp :: (MonadMutagen m) => MutagenState -> String -> m Report
giveUp st gaveUpReason = do
  message $ "Gave up: " <> gaveUpReason
  return
    GaveUp
      { reason = gaveUpReason
      , numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- | Found a bug!
counterexample :: (MonadMutagen m) => MutagenState -> Args -> Result -> m Report
counterexample st args result = do
  message "Found counterexample!"
  pretty args
  message "*** Reason of failure:"
  case resultReason result of
    Just failureReason -> message failureReason
    Nothing -> message "assertion failed"
  case resultException result of
    Just exc -> do
      message "*** The exception was:"
      message (show exc)
    Nothing -> return ()
  return
    Counterexample
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      , failingArgs = args
      }

-- | Expected failure did not occur
noExpectedFailure :: (MonadMutagen m) => MutagenState -> m Report
noExpectedFailure st = do
  message "Expected failure did not occur!"
  return
    NoExpectedFailure
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- ** Running tests

-- Select a new test, mutating and existing interesting one or generating a
-- brand new otherwise.
pickNextTestCase
  :: (MonadMutagen m)
  => MutagenState
  -> m (Args, Maybe (MutationBatch Args), MutagenState)
pickNextTestCase st
  -- We can run a mutation of an interesting succesful test case
  | mutationQueueSize (stPassedQueue st) > 0 = mutateFromPassed st
  -- We can run a mutation of an interesting discarded test case
  | mutationQueueSize (stDiscardedQueue st) > 0 = mutateFromDiscarded st
  -- Only choice left is to generate a brand new test
  | otherwise = generateNewTest st

-- | Generate a brand new test case
generateNewTest
  :: (MonadMutagen m)
  => MutagenState
  -> m (Args, Maybe (MutationBatch Args), MutagenState)
generateNewTest st = do
  -- First we compute an appropriate generation size
  let size = computeSize st
  -- Then we randomly generate a lazily evaluated test
  let (rnd1, rnd2) = split (stNextSeed st)
  let args = unGen (stArgsGen st) rnd1 size
  when (stChatty st) $ do
    message "Generated test case"
    pretty args
  -- Put the new random seed in the state
  let st' =
        st
          & setNextSeed rnd2
          & setCurrentGenSize size
          & incNumGenerated
  return (args, Nothing, st')

-- | Mutate a test case from the passed queue
mutateFromPassed
  :: (MonadMutagen m)
  => MutagenState
  -> m (Args, Maybe (MutationBatch Args), MutagenState)
mutateFromPassed st = do
  let (prio, candidate, rest) = dequeueNextMutationCandidate (stPassedQueue st)
  next <- liftIO $ nextMutation (stFragmentStore st) (mcBatch candidate)
  case next of
    Nothing -> do
      let st' = st & setPassedQueue rest
      pickNextTestCase st'
    Just (args, kind, batch) -> do
      when (stChatty st) $ do
        message "Mutating from passed test case:"
        pretty (mcArgs candidate)
        message "Mutated test case:"
        pretty args
        printBatchStatus batch
      let st' =
            st
              & setPassedQueue
                (enqueueMutationCandidate prio candidate{mcBatch = batch} rest)
              & incMutantKindCounter kind
              & incNumMutatedFromPassed
      return (args, Just batch, st')

-- | Mutate a test case from the discarded queue
mutateFromDiscarded
  :: (MonadMutagen m)
  => MutagenState
  -> m (Args, Maybe (MutationBatch Args), MutagenState)
mutateFromDiscarded st = do
  let (prio, candidate, rest) = dequeueNextMutationCandidate (stDiscardedQueue st)
  next <- liftIO $ nextMutation (stFragmentStore st) (mcBatch candidate)
  case next of
    Nothing -> do
      let st' = st & setDiscardedQueue rest
      pickNextTestCase st'
    Just (args, kind, batch) -> do
      when (stChatty st) $ do
        message "Mutating from discarded test case:"
        pretty (mcArgs candidate)
        message "Mutated test case:"
        pretty args
        printBatchStatus batch
      let st' =
            st
              & setDiscardedQueue
                (enqueueMutationCandidate prio candidate{mcBatch = batch} rest)
              & incMutantKindCounter kind
              & incNumMutatedFromDiscarded
      return (args, Just batch, st')

-- | Run a test case and update the internal state accordingly
runTestCase
  :: (MonadMutagen m)
  => Args
  -- ^ Test case
  -> Maybe (MutationBatch Args)
  -- ^ Parent mutation batch
  -> MutagenState
  -- ^ Current state
  -> m (Result, MutagenState)
runTestCase args parent st = do
  when (stChatty st) $ do
    message "Running test case..."
  -- Run the test case
  (result, trace, evaluatedPos) <- liftIO $ execPropRunner st args
  -- Truncate the trace if necessary
  let trace' = maybe trace (flip truncateTrace trace) (stMaxTraceLength st)
  when (stChatty st) $ do
    case evaluatedPos of
      Just pos -> do
        message "Evaluated subexpressions:"
        pretty pos
      Nothing -> do
        return () -- lazy prunning is disabled
        -- Print the trace of the mutated test case
    message "Test case trace:"
    pretty (unTrace trace')
  -- Extract property modifiers
  let addPropertyModifiers =
        setExpect (resultExpect result)
  -- Inspect the test result
  case result of
    -- Boom!
    Failed -> do
      when (stChatty st) $ do
        message "Test result: FAILED"
      -- Report the counterexample
      return
        ( result
        , addPropertyModifiers st
        )
    -- Test passed, lotta work to do now
    Passed -> do
      when (stChatty st) $ do
        message "Test result: PASSED"
      -- Save the trace in the corresponding trace store
      (new, prio) <- liftIO (savePassedTraceWithPrio st trace')
      -- Evaluate whether the test case was interesting or not depending on
      -- whether it added new trace nodes or not
      let interesting = new > 0
      -- From here, we will update the internal state for the next iteration
      -- depending on whether the test case was interesting or not
      st' <-
        if interesting
          then do
            when (stChatty st) $ do
              message "Test case was interesting!"
            let candidate = createMutationCandidate trace evaluatedPos True
            return
              $ st
                & storeFragmentsIfStoreIsEnabled args
                & enqueuePassedCandidate prio candidate
                & incNumInteresting
                & resetNumTestsSinceLastInteresting
          else do
            return
              $ st
                & incNumBoring
                & incNumTestsSinceLastInteresting
      -- Finally, return the updated state and apply other updates
      return
        ( result
        , st'
            & addPropertyModifiers
            & incNumPassed
        )
    -- Test discarded, lotta work to do here too
    Discarded -> do
      when (stChatty st) $ do
        message "Test result: DISCARDED"
      -- Save the trace in the corresponding trace log
      (new, prio) <- liftIO (saveDiscardedTraceWithPrio st trace')
      -- Evaluate whether the test case was interesting or not
      --
      -- NOTE: in this case, we only consider discarded test cases interesting
      -- if its parent test case, i.e., the one it was mutated from, also
      -- passed the property and was not discarded.
      let interesting = new > 0 && maybe False mbTestPassed parent
      when (stChatty st && interesting) $ do
        message "Test case was interesting!"
      -- As above, here we also update the internal state depending on whether
      -- the test case was interesting or not
      st' <-
        if interesting
          then do
            let candidate = createMutationCandidate trace evaluatedPos False
            return
              $ st
                & storeFragmentsIfStoreIsEnabled args
                & enqueueDiscardedCandidate prio candidate
                & incNumInteresting
                & resetNumTestsSinceLastInteresting
          else do
            return
              $ st
                & incNumBoring
                & incNumTestsSinceLastInteresting
      -- Finally, return the updated state and apply other updates
      return
        ( result
        , st'
            & addPropertyModifiers
            & incNumDiscarded
        )
  where
    -- Create a mutation candidate from extracted runtime data
    createMutationCandidate trace evaluatedPos isPassed =
      MutationCandidate
        args
        trace
        ( createOrInheritMutationBatch
            (stMutationOrder st)
            (stRandomMutations st)
            (stMaxGenSize st)
            (stRandomFragments st)
            (stMutationLimit st)
            args
            parent
            evaluatedPos
            isPassed
        )
    -- Enqueue a candidate into the passed queue
    enqueuePassedCandidate prio candidate =
      updatePassedQueue
        ( enqueueMutationCandidate
            prio
            candidate
        )
    -- Enqueue a candidate into the discarded queue
    enqueueDiscardedCandidate prio candidate =
      updatePassedQueue
        ( enqueueMutationCandidate
            prio
            candidate
        )
    -- Store fragments if the fragment store is enabled
    storeFragmentsIfStoreIsEnabled args' st'
      | stUseFragments st' =
          updateFragmentStore
            (storeFragments (stFilterFragments st') args')
            st'
      | otherwise =
          st'

-- ** IO helpers

-- | Execute a test and capture:
--   * The test result (passed, discarded, failed)
--   * The trace in the program it traversed
--   * The positions of the evaluated expressions of the input
--     (if lazy prunning is currently enabled)
execPropRunner :: MutagenState -> Args -> IO (Result, Trace, Maybe [Pos])
execPropRunner st args
  | stUseLazyPrunning st = do
      resetPosRef
      (test, trace) <- withTrace (unProp (protectProp (stPropRunner st (lazy args))))
      evaluated <- readPosRef
      return (test, trace, Just evaluated)
  | otherwise = do
      (test, trace) <- withTrace (unProp (protectProp (stPropRunner st args)))
      return (test, trace, Nothing)

-- | Save a discarded trace and return the number of new nodes added and its
-- the priority associated to its corresponding test case.
savePassedTraceWithPrio :: MutagenState -> Trace -> IO (Int, Int)
savePassedTraceWithPrio st tr =
  case st of
    MutagenState{stTraceType = SBitmap, stPassedTraceLog = store} -> do
      new <- saveTrace tr store
      let prio = stNumTracingNodes st - new
      return (new, prio)
    MutagenState{stTraceType = STree, stPassedTraceLog = store} -> do
      (new, depth) <- saveTrace tr store
      let prio = depth
      return (new, prio)

-- | Save a discarded trace and return the number of new nodes added and its
-- the priority associated to its corresponding test case.
saveDiscardedTraceWithPrio :: MutagenState -> Trace -> IO (Int, Int)
saveDiscardedTraceWithPrio st tr =
  case st of
    MutagenState{stTraceType = SBitmap, stDiscardedTraceLog = store} -> do
      new <- saveTrace tr store
      let prio = stNumTracingNodes st - new
      return (new, prio)
    MutagenState{stTraceType = STree, stDiscardedTraceLog = store} -> do
      (new, depth) <- saveTrace tr store
      let prio = depth
      return (new, prio)

-- | Run a computation using the passed trace store
withPassedTraceStore
  :: MutagenState
  -> (forall trace. (TraceStoreImpl trace) => TraceStore trace -> r)
  -> r
withPassedTraceStore st k =
  case st of MutagenState{stPassedTraceLog = store} -> k store

-- | Run a computation using the discarded trace store
withDiscardedTraceStore
  :: MutagenState
  -> (forall trace. (TraceStoreImpl trace) => TraceStore trace -> r)
  -> r
withDiscardedTraceStore st k =
  case st of MutagenState{stDiscardedTraceLog = store} -> k store
