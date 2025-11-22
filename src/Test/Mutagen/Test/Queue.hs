{-# LANGUAGE RankNTypes #-}

-- | Queues keeping a prioritized collection of test cases to mutate next
module Test.Mutagen.Test.Queue
  ( -- * Mutation queues
    MutationQueue (..)
  , emptyMutationQueue
  , mutationQueueSize
  , enqueueMutationCandidate
  , dequeueNextMutationCandidate

    -- * Mutation candidates
  , MutationCandidate (..)

    -- * Mutation batches
  , MutationBatch (..)
  , newMutationBatch
  , newMutationBatchFromParent
  , createOrInheritMutationBatch
  , nextMutation
  )
where

import Control.Monad.Extra (concatMapM)
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue
import Test.Mutagen.Fragment (FragmentStore)
import Test.Mutagen.Mutant (Concretized (..), MutantKind, concretize)
import Test.Mutagen.Mutation (Mutable (..), MutationOrder, Pos)
import Test.Mutagen.Property (Args)
import Test.Mutagen.Tracer.Trace (Trace)

{-------------------------------------------------------------------------------
-- * Mutation queues
-------------------------------------------------------------------------------}

-- | Mutation queue priority
type Priority = Int

-- | Mutation queues storing prioritized mutation candidates
newtype MutationQueue
  = MutationQueue (MinPQueue Priority (MutationCandidate Args))

-- | Empty mutation queue
emptyMutationQueue :: MutationQueue
emptyMutationQueue = MutationQueue mempty

-- | Size of a mutation queue
mutationQueueSize :: MutationQueue -> Int
mutationQueueSize (MutationQueue q) = PQueue.size q

-- | Enqueue a test case into a mutation queue with a given priority
enqueueMutationCandidate
  :: Priority
  -> MutationCandidate Args
  -> MutationQueue
  -> MutationQueue
enqueueMutationCandidate prio candidate (MutationQueue queue) =
  MutationQueue (PQueue.insert prio candidate queue)

-- | Dequeue the next test case from a mutation queue according to its priority
dequeueNextMutationCandidate
  :: MutationQueue
  -> (Priority, MutationCandidate Args, MutationQueue)
dequeueNextMutationCandidate (MutationQueue queue) =
  let ((prio, candidate), queue') = PQueue.deleteFindMin queue
   in (prio, candidate, MutationQueue queue')

{-------------------------------------------------------------------------------
-- * Mutation candidates
-------------------------------------------------------------------------------}

-- | Mutation candidates
--
-- A mutation candidate is a previously executed test case that was found
-- to be interesting according to its execution trace. Such test cases are
-- stored in a mutation queue to be later used as seeds for generating
-- new test cases via mutation.
data MutationCandidate args
  = MutationCandidate
  { mcArgs :: args
  -- ^ Test case arguments
  , mcTrace :: Trace
  -- ^ Execution trace of the test case
  , mcBatch :: MutationBatch args
  -- ^ Mutation batch associated to the test case
  }

{-------------------------------------------------------------------------------
-- * Mutation batches
-------------------------------------------------------------------------------}

-- | Mutation batches
--
-- In contrast to mutation queues, which simultaneously store multiple test
-- cases to be mutated, mutation batches store the state of mutating a single
-- test case.
data MutationBatch args
  = MutationBatch
  { mbArgs :: args
  -- ^ Original test case arguments
  , mbPastPositions :: [Pos]
  -- ^ Positions already mutated
  , mbCurrBatch :: [Concretized args]
  -- ^ Current queue of mutants for the current position
  , mbNextPositions :: [Pos]
  -- ^ Positions yet to be mutated
  , mbMutationOrder :: MutationOrder
  -- ^ Mutation order to use
  , mbTestPassed :: Bool
  -- ^ Whether the original test case passed the property or was discarded
  , mbRandomMutationSize :: Int
  -- ^ Maximum generation size for random mutations
  , mbNumRandomMutations :: Int
  -- ^ Number of test cases to randomly sample per random mutant
  , mbNumFragMutations :: Int
  -- ^ Number of test cases to sample from the store per fragment mutant
  , mbMaxMutationDepth :: Int
  -- ^ Maximum mutation depth remaining
  }

-- | Create a new mutation batch from scratch
newMutationBatch
  :: (Mutable a)
  => MutationOrder
  -> Int
  -> Int
  -> Int
  -> Int
  -> Maybe [Pos]
  -> Bool
  -> a
  -> MutationBatch a
newMutationBatch
  mutationOrder
  numRandomMutations
  randomMutationSize
  numFragMutations
  maxMutationDepth
  evaluatedPositions
  testPassed
  args =
    MutationBatch
      { mbArgs = args
      , mbPastPositions = mempty
      , mbNextPositions = nextPositions
      , mbCurrBatch = mempty
      , mbMutationOrder = mutationOrder
      , mbTestPassed = testPassed
      , mbRandomMutationSize = randomMutationSize
      , mbNumRandomMutations = numRandomMutations
      , mbMaxMutationDepth = maxMutationDepth
      , mbNumFragMutations = numFragMutations
      }
    where
      -- Determine next positions to mutate based on whether we receive the
      -- concrete execution trace of the original
      nextPositions =
        fromMaybe (mutationOrder (positions args)) evaluatedPositions

-- | Create a new mutation batch by inheriting from a parent one
--
-- NOTE: this decreases the maximum mutation depth of the parent batch by one.
newMutationBatchFromParent
  :: (Mutable a)
  => MutationBatch a
  -> Maybe [Pos]
  -> Bool
  -> a
  -> MutationBatch a
newMutationBatchFromParent
  batch
  evaluatedPositions
  testPassed
  args =
    batch
      { mbArgs = args
      , mbNextPositions = nextPositions
      , mbPastPositions = mempty
      , mbTestPassed = testPassed
      , mbCurrBatch = mempty
      , mbMaxMutationDepth = mbMaxMutationDepth batch - 1
      }
    where
      -- Determine next positions to mutate based on whether we receive the
      -- concrete execution trace of the original
      nextPositions =
        fromMaybe (mbMutationOrder batch (positions args)) evaluatedPositions

-- | Create or inherit mutation batch for a test case.
--
-- If the test case was generated by mutating an existing one, we can inherit
-- its parent mutation batch and just update it with the new test case.
-- Otherwise, we need to create a new mutation batch from scratch.
createOrInheritMutationBatch
  :: MutationOrder
  -- ^ Mutation order to use
  -> Int
  -- ^ Maximum number of random mutations to sample per mutant
  -> Int
  -- ^ Maximum generation size for random mutations
  -> Int
  -- ^ Number of random fragments to sample from the fragment store
  -> Int
  -- ^ Maximum mutation limit for the test case
  -> Args
  -- ^ Test case arguments
  -> Maybe (MutationBatch Args)
  -- ^ Parent test case mutation batch to derive a new one from, if any
  -> Maybe [Pos]
  -- ^ Mutation positions available in the test case
  -> Bool
  -- ^ Whether the test case passed the property or was discarded
  -> MutationBatch Args
createOrInheritMutationBatch
  mutationOrder
  randomMutations
  maxGenSize
  randomFragments
  maxMutationDepth
  args
  parentBatch
  pos
  isPassed =
    case parentBatch of
      -- The test case was mutated from an existing one
      Just mb ->
        newMutationBatchFromParent
          mb
          pos
          isPassed
          args
      -- The test case was freshly generated
      Nothing ->
        newMutationBatch
          mutationOrder
          randomMutations
          maxGenSize
          randomFragments
          maxMutationDepth
          pos
          isPassed
          args

-- | Compute the next mutation from a mutation batch
nextMutation
  :: (Mutable a)
  => FragmentStore
  -> MutationBatch a
  -> IO (Maybe (a, MutantKind, MutationBatch a))
nextMutation _ batch
  | mbMaxMutationDepth batch == 0 = return Nothing -- too many mutations
nextMutation fs batch = do
  case mbCurrBatch batch of
    -- Queue is empty, advance to next position
    [] -> do
      case mbNextPositions batch of
        -- No more positions to mutate
        [] -> return Nothing
        -- Next position available
        pos : ps -> do
          let mutants = inside pos mutate (mbArgs batch)
          queue <-
            concatMapM
              ( concretize
                  (mbNumRandomMutations batch, mbRandomMutationSize batch)
                  (mbNumRandomMutations batch, fs)
              )
              mutants
          case queue of
            -- Current position admits no mutations: advance to next position
            [] -> do
              let mb' =
                    batch
                      { mbNextPositions = ps
                      , mbPastPositions = pos : mbPastPositions batch
                      }
              nextMutation fs mb'
            -- Current position admits some mutations: update the batch queue
            -- and lock the current position
            Concretized kind a : as -> do
              let mb' =
                    batch
                      { mbNextPositions = ps
                      , mbPastPositions = pos : mbPastPositions batch
                      , mbCurrBatch = as
                      }
              return (Just (a, kind, mb'))
    -- There are some mutants still in the queue for the current position
    Concretized kind a : as -> do
      return (Just (a, kind, batch{mbCurrBatch = as}))
