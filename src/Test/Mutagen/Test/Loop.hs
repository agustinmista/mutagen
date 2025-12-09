{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Mutagen's main testing loop.
module Test.Mutagen.Test.Loop
  ( -- * Test loop
    loop
  )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import System.Random (split)
import Test.Mutagen.Config
  ( DebugMode (..)
  , EvaluationOrder (..)
  , LazyPruningMode (..)
  )
import Test.Mutagen.Fragment.Store (storeFragments)
import Test.Mutagen.Lazy (withLazyIO)
import Test.Mutagen.Mutation (Pos)
import Test.Mutagen.Property
  ( Args
  , Result (..)
  , protectProp
  , resultException
  , resultExpect
  , unProp
  , pattern Discarded
  , pattern Failed
  , pattern Passed
  )
import Test.Mutagen.Report
  ( Report (..)
  )
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
  ( MutagenState (..)
  , computeSize
  , incMutantKindCounter
  , incNumBoring
  , incNumDiscarded
  , incNumFailed
  , incNumGenerated
  , incNumInteresting
  , incNumMutatedFromDiscarded
  , incNumMutatedFromPassed
  , incNumPassed
  , incNumTestsSinceLastInteresting
  , incNumTraceStoreResets
  , nextCounterexamplePath
  , resetNumTestsSinceLastInteresting
  , setAutoResetAfter
  , setCurrentGenSize
  , setDiscardedQueue
  , setExpect
  , setNextSeed
  , setPassedQueue
  , setRandomMutations
  , timedOut
  , updateFragmentStore
  , updatePassedQueue
  )
import Test.Mutagen.Test.Terminal (MonadTerminal (..))
import Test.Mutagen.Tracer.Store (STraceBackend (..), TraceStoreImpl (..))
import Test.Mutagen.Tracer.Trace (Trace (..), truncateTrace, withTrace)
import Test.QuickCheck.Gen (unGen)

{-------------------------------------------------------------------------------
-- * The main test loop
-------------------------------------------------------------------------------}

-- | Constraint alias for monads that can run Mutagen tests.
type MonadMutagen m = (MonadIO m, MonadTerminal m)

-- | The entry point to Mutagen's main testing loop.
--
-- The state machine switches back and forth between 'loop' and 'newTest',
-- until one of the terminal conditions is met.
loop :: (MonadMutagen m) => MutagenState -> m Report
loop st
  -- We reached the max number of tests
  -- ==> either success or expected failure did not occur
  | stNumPassed st >= stMaxSuccess st =
      case (stExpect st, stKeepGoing st) of
        (_, True) -> success st -- keepGoing always returns success
        (True, _) -> success st -- property holds as expected
        (False, _) -> noExpectedFailure st -- expected failure did not occur
        -- We discarded too many tests
        -- ==> give up
  | stNumDiscarded st
      >= stMaxDiscardRatio st * max (stNumPassed st) (stMaxSuccess st)
      && not (stKeepGoing st) =
      giveUp st "too many discarded tests"
  -- Time to check if the time budget has been exceeded
  -- ==> if so, either success or give up depending on 'stKeepGoing'
  -- ==> otherwise, continue testing
  | (stNumPassed st + stNumDiscarded st + stNumFailed st) `mod` 100 == 0 = do
      timeout <- liftIO (timedOut st)
      case (timeout, stKeepGoing st) of
        (True, True) -> success st
        (True, False) -> giveUp st "timeout"
        _ -> newTest st
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
              & incNumTraceStoreResets
              & resetNumTestsSinceLastInteresting
      newTest st'
  -- Nothing new under the sun
  -- ==> continue testing
  | otherwise =
      newTest st

-- | Select the next test case, run it, and process the result.
newTest :: (MonadMutagen m) => MutagenState -> m Report
newTest st0 = do
  cleanLog
  -- NOTE: using 'stN' to avoid bugs related to tildes in the helpers below
  -- 1. pick a new test case
  (args, parent, st1) <- pickNextTestCase st0
  -- 2. run the test case
  (result, st2) <- runTestCase args parent st1
  -- 3. check the test result and report a counterexample or continue
  case result of
    Failed -> onFailed st2 args result
    _ -> onSuccessOrDiscarded st2 args result
  where
    -- What to do with a successful or discarded test case
    onSuccessOrDiscarded st _args result = do
      printStats (stChatty st) st
      stopOnDebugMode (stDebug st) result
      loop st
    -- What to do with a failed test case
    onFailed st args result = do
      let st' = st & incNumFailed
      reportCounterexample st' args result
      printStats (stChatty st') st'
      stopOnDebugMode (stDebug st') result
      stopOrKeepGoing st' args
    -- Stop execution if in debug mode
    stopOnDebugMode debugMode res =
      case debugMode of
        StopOnPassed | Passed <- res -> awaitForUserInput
        AlwaysStop -> awaitForUserInput
        _ -> return ()
    awaitForUserInput = do
      void $ readLine "Press enter to continue ..."
    -- Stop or continue after a failed test case
    stopOrKeepGoing st args
      -- Check if we should keep going
      | stKeepGoing st = do
          message
            $ "Failed "
              <> show (stNumFailed st)
              <> " times, keeping going..."
          loop st
      -- Check if this was an expected failure and mask the report as success
      | not (stExpect st) =
          success st
      -- Otherwise, report the counterexample
      | otherwise =
          counterexample st args

-- | Report a found counterexample.
reportCounterexample
  :: (MonadMutagen m)
  => MutagenState
  -> Args
  -> Result
  -> m ()
reportCounterexample st args result = do
  message "Found counterexample!"
  pretty args
  message "Reason of failure:"
  case resultReason result of
    Just failureReason -> message failureReason
    Nothing -> message "assertion failed"
  case resultException result of
    Just exc -> do
      message "The exception was:"
      message (show exc)
    Nothing -> return ()
  case nextCounterexamplePath st of
    Just path -> do
      message $ "Saving counterexample to: " <> path
      liftIO $ writeFile path (show args)
    Nothing -> return ()

-- * Terminal states

-- | All tests passed successfully.
success :: (MonadMutagen m) => MutagenState -> m Report
success st = do
  message "Done testing"
  return
    Success
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      , numFailed = stNumFailed st
      }

-- | Found a counterexample!
counterexample :: (MonadMutagen m) => MutagenState -> Args -> m Report
counterexample st args = do
  message
    $ "Property falsified after "
      <> show (stNumPassed st)
      <> " passed tests and "
      <> show (stNumDiscarded st)
      <> " discarded tests."
  return
    Counterexample
      { numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      , failingArgs = args
      }

-- | Too many discarded tests.
giveUp :: (MonadMutagen m) => MutagenState -> String -> m Report
giveUp st gaveUpReason = do
  message $ "Gave up: " <> gaveUpReason
  return
    GaveUp
      { reason = gaveUpReason
      , numPassed = stNumPassed st
      , numDiscarded = stNumDiscarded st
      }

-- | Expected failure did not occur.
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

-- | Generate a brand new test case.
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

-- | Mutate a test case from the passed queue.
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
        printBatch batch
      let st' =
            st
              & setPassedQueue
                (enqueueMutationCandidate prio candidate{mcBatch = batch} rest)
              & incMutantKindCounter kind
              & incNumMutatedFromPassed
      return (args, Just batch, st')

-- | Mutate a test case from the discarded queue.
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
        printBatch batch
      let st' =
            st
              & setDiscardedQueue
                (enqueueMutationCandidate prio candidate{mcBatch = batch} rest)
              & incMutantKindCounter kind
              & incNumMutatedFromDiscarded
      return (args, Just batch, st')

-- | Run a test case and update the internal state accordingly.
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
  when (stChatty st) $ do
    case evaluatedPos of
      Just pos -> do
        message "Evaluated subexpressions:"
        pretty pos
      Nothing -> do
        return () -- lazy prunning is disabled
        -- Print the trace of the mutated test case
    message "Test case trace:"
    pretty (unTrace trace)
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
      (new, prio) <- liftIO (savePassedTraceWithPrio st trace)
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
      (new, prio) <- liftIO (saveDiscardedTraceWithPrio st trace)
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
            (stMaxMutationDepth st)
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
--   * The (possibly truncated) execution trace in the program it traversed
--   * The positions of the evaluated subexpressions of the input, in the order
--     that they need to be mutated (only when lazy pruning is enabled).
execPropRunner :: MutagenState -> Args -> IO (Result, Trace, Maybe [Pos])
execPropRunner st args
  | LazyPruning order <- stLazyPruning st = do
      (evaluated, (result, trace)) <- withLazyIO (withTrace . runProp) args
      return
        ( result
        , truncateTraceIfNeeded trace
        , Just (withMutationOrder order evaluated)
        )
  | otherwise = do
      (result, trace) <- withTrace (runProp args)
      return
        ( result
        , truncateTraceIfNeeded trace
        , Nothing
        )
  where
    runProp = unProp . protectProp . stPropRunner st
    withMutationOrder order =
      case order of
        Forward -> id
        Backward -> reverse
    truncateTraceIfNeeded trace =
      maybe trace (flip truncateTrace trace) (stMaxTraceLength st)

-- | Save a discarded trace and return the number of new nodes added and its
-- the priority associated to its corresponding test case.
savePassedTraceWithPrio :: MutagenState -> Trace -> IO (Int, Int)
savePassedTraceWithPrio st tr =
  case st of
    MutagenState{stTraceBackend = SBitmap, stPassedTraceStore = store} -> do
      new <- saveTrace tr store
      let prio = stNumTracingNodes st - new
      return (new, prio)
    MutagenState{stTraceBackend = STree, stPassedTraceStore = store} -> do
      (new, depth) <- saveTrace tr store
      let prio = depth
      return (new, prio)

-- | Save a discarded trace and return the number of new nodes added and its
-- the priority associated to its corresponding test case.
saveDiscardedTraceWithPrio :: MutagenState -> Trace -> IO (Int, Int)
saveDiscardedTraceWithPrio st tr =
  case st of
    MutagenState{stTraceBackend = SBitmap, stDiscardedTraceStore = store} -> do
      new <- saveTrace tr store
      let prio = stNumTracingNodes st - new
      return (new, prio)
    MutagenState{stTraceBackend = STree, stDiscardedTraceStore = store} -> do
      (new, depth) <- saveTrace tr store
      let prio = depth
      return (new, prio)

-- | Run a computation using the passed trace store.
withPassedTraceStore
  :: MutagenState
  -> (forall trace. (TraceStoreImpl trace) => TraceStore trace -> r)
  -> r
withPassedTraceStore st k =
  case st of MutagenState{stPassedTraceStore = store} -> k store

-- | Run a computation using the discarded trace store.
withDiscardedTraceStore
  :: MutagenState
  -> (forall trace. (TraceStoreImpl trace) => TraceStore trace -> r)
  -> r
withDiscardedTraceStore st k =
  case st of MutagenState{stDiscardedTraceStore = store} -> k store
