{-# LANGUAGE RankNTypes #-}

module Test.Mutagen.Test.State where

import Data.Typeable

import Data.PQueue.Prio.Min (MinPQueue)
import Data.Time.Clock.POSIX

import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Random (QCGen, newQCGen)

import Test.Mutagen.Tracer
import Test.Mutagen.Property
import Test.Mutagen.Mutation
import Test.Mutagen.Mutant
import Test.Mutagen.Fragment
import Test.Mutagen.Test.Config
import Test.Mutagen.Test.Batch

----------------------------------------
-- | Testing state (internal)

data State log
  = State
  ----------------------------------------
  -- Mirrored from Config
  { stMaxSuccess :: !Int
  , stMaxDiscardRatio :: !Int
  , stTimeout :: !(Maybe Integer)
  , stMaxGenSize :: !Int
  , stUseLIFO :: !Bool
  , stRandomMutations :: !Int
  , stMutationLimit :: !Int
  , stAutoResetAfter :: !(Maybe Int)
  , stUseLazyPrunning :: !Bool
  , stMutationOrder :: !MutationOrder
  , stRandomFragments :: !Int
  , stUseFragments :: !Bool
  , stFilterFragments :: !(Maybe [TypeRep])
  , stMaxTraceLength :: !(Maybe Int)
  , stChatty :: !Bool
  , stDebug :: !Bool

  ----------------------------------------
  -- Test case generation
  , stNextSeed :: !QCGen
  -- ^ The next seed to be used by `stArgsGen`
  , stArgsGen :: !(Gen Args)
  -- ^ The random generator of inputs

  ----------------------------------------
  -- Property runner
  , stArgsRunner :: !(Args -> Result)

  ----------------------------------------
  -- Trace logs
  , stGeneratedTraceNodes :: !Int
  , stPassedTraceLog :: !log
  , stDiscardedTraceLog :: !log

  ----------------------------------------
  -- Mutation queues
  , stPassedQueue :: !MutationQueue
  , stDiscardedQueue :: !MutationQueue

  ----------------------------------------
  -- Global fragment store
  , stFragmentStore :: !FragmentStore

  ----------------------------------------
  -- Statistics
  , stStartTime :: !Integer
  , stCurrentGenSize :: !Int
  , stNumGenerated :: !Int
  , stNumMutatedFromPassed :: !Int
  , stNumMutatedFromDiscarded :: !Int
  , stNumPureMutants :: !Int
  , stNumRandMutants :: !Int
  , stNumFragMutants :: !Int
  , stNumPassed :: !Int
  , stNumDiscarded :: !Int
  , stNumInteresting :: !Int
  , stNumBoring :: !Int
  , stNumTestsSinceLastInteresting :: !Int
  , stNumTraceLogResets :: !Int
  }

createInitialState :: forall log. TraceLogger log => Config -> Property -> IO (State log)
createInitialState cfg (Property gen argsRunner) = do
  -- rng generator
  rng <- newQCGen
  -- start timestamp
  now <- round <$> getPOSIXTime
  -- read the amount of trace nodes generated by the plugin
  traceNodes <- read <$> readFile ".tracer"
  -- create empty trace logs for passed and discarded test cases
  passedTraceLog <- emptyTraceLog traceNodes
  discardedTraceLog <- emptyTraceLog traceNodes
  -- fragmentize the initial example seeds
  let initialFragmentStore = if useFragments cfg
                             then foldr (storeFragments (filterFragments cfg)) emptyFragmentStore (examples cfg)
                             else emptyFragmentStore
  -- build the initial state
  return State
    -- From config
    { stMaxSuccess = maxSuccess cfg
    , stMaxDiscardRatio = maxDiscardRatio cfg
    , stTimeout = timeout cfg
    , stMaxGenSize = maxGenSize cfg
    , stUseLIFO = useLIFO cfg
    , stRandomMutations = randomMutations cfg
    , stRandomFragments = randomFragments cfg
    , stMutationLimit = maybe (maxGenSize cfg) id (mutationLimit cfg)
    , stAutoResetAfter = autoResetAfter cfg
    , stUseLazyPrunning = useLazyPrunning cfg
    , stMutationOrder = mutationOrder cfg
    , stUseFragments = useFragments cfg
    , stFilterFragments = filterFragments cfg
    , stMaxTraceLength = maxTraceLength cfg
    , stChatty = chatty cfg || debug cfg
    , stDebug = debug cfg
    -- Internal
    , stNextSeed = rng
    , stArgsGen = gen
    , stArgsRunner = argsRunner
    , stGeneratedTraceNodes = traceNodes
    , stPassedTraceLog = passedTraceLog
    , stDiscardedTraceLog = discardedTraceLog
    , stPassedQueue = mempty
    , stDiscardedQueue = mempty
    , stFragmentStore = initialFragmentStore
    , stStartTime = now
    , stCurrentGenSize = 0
    , stNumGenerated = 0
    , stNumMutatedFromPassed = 0
    , stNumMutatedFromDiscarded = 0
    , stNumPureMutants = 0
    , stNumRandMutants = 0
    , stNumFragMutants = 0
    , stNumPassed = 0
    , stNumDiscarded = 0
    , stNumInteresting = 0
    , stNumBoring = 0
    , stNumTestsSinceLastInteresting = 0
    , stNumTraceLogResets = 0
    }

----------------------------------------
-- State modifiers

-- Reverse dollar sign
(!) :: State log -> (State log -> State log) -> State log
st ! f = f st

infixl 2 !

setNextSeed :: QCGen -> State log -> State log
setNextSeed val st = st { stNextSeed = val }

setCurrentGenSize :: Int -> State log -> State log
setCurrentGenSize val st = st { stCurrentGenSize = val }

setAutoResetAfter :: Maybe Int -> State log -> State log
setAutoResetAfter val st = st { stAutoResetAfter = val }

setRandomMutations :: Int -> State log -> State log
setRandomMutations val st = st { stRandomMutations = val }

setPassedQueue :: MutationQueue -> State log -> State log
setPassedQueue    val st = st { stPassedQueue = val }

setDiscardedQueue :: MutationQueue -> State log -> State log
setDiscardedQueue val st = st { stDiscardedQueue = val }

setFragmentStore :: FragmentStore -> State log -> State log
setFragmentStore  val st = st { stFragmentStore = val }

increaseNumTraceLogResets :: State log -> State log
increaseNumTraceLogResets st = st { stNumTraceLogResets = stNumTraceLogResets st + 1 }

increaseNumPassed :: State log -> State log
increaseNumPassed st = st { stNumPassed = stNumPassed st + 1 }

increaseNumDiscarded :: State log -> State log
increaseNumDiscarded st = st { stNumDiscarded = stNumDiscarded st + 1 }

increaseNumGenerated :: State log -> State log
increaseNumGenerated st = st { stNumGenerated = stNumGenerated st + 1 }

increaseNumMutatedFromPassed :: State log -> State log
increaseNumMutatedFromPassed st = st { stNumMutatedFromPassed = stNumMutatedFromPassed st + 1 }

increaseNumMutatedFromDiscarded :: State log -> State log
increaseNumMutatedFromDiscarded st = st { stNumMutatedFromDiscarded = stNumMutatedFromDiscarded st + 1 }

increaseNumInteresting :: State log -> State log
increaseNumInteresting st = st { stNumInteresting = stNumInteresting st + 1 }

increaseNumBoring :: State log -> State log
increaseNumBoring st = st { stNumBoring = stNumBoring st + 1 }

increaseNumTestsSinceLastInteresting :: State log -> State log
increaseNumTestsSinceLastInteresting st = st { stNumTestsSinceLastInteresting = stNumTestsSinceLastInteresting st + 1 }

increaseMutantKindCounter :: MutantKind -> State log -> State log
increaseMutantKindCounter PureMutant st = st { stNumPureMutants = stNumPureMutants st + 1 }
increaseMutantKindCounter RandMutant st = st { stNumRandMutants = stNumRandMutants st + 1 }
increaseMutantKindCounter FragMutant st = st { stNumFragMutants = stNumFragMutants st + 1 }

resetNumTestsSinceLastInteresting :: State log -> State log
resetNumTestsSinceLastInteresting st = st { stNumTestsSinceLastInteresting = 0 }

----------------------------------------
-- Mutation priority queues

-- Mutation candidates
type MutationQueue =
  MinPQueue
    Int                  -- The candidate priority
    ( Args               -- The test case inputs
    , Trace              -- The code coverage it triggered
    , MutationBatch Args -- The batch of possible mutations
    )

-- Inherit the mutation batch of a parent test case of it exists, otherwise create a new one.
createOrInheritMutationBatch :: State log -> Args -> Maybe (MutationBatch Args) -> Maybe [Pos] -> Bool -> MutationBatch Args
createOrInheritMutationBatch st args parentbatch pos isPassed =
  case parentbatch of
    -- test case was mutated from an existing one, we can augment its parent mutation batch
    Just mb ->
      newMutationBatchFromParent mb
        pos isPassed args
    -- test case was freshly generated, we need to initialize a new mutation batch
    Nothing ->
      newMutationBatch
        (stMutationOrder st)
        (stRandomMutations st)
        (stMaxGenSize st)
        (stRandomFragments st)
        (stMutationLimit st)
        pos isPassed args

----------------------------------------
-- State-related utilities

-- Check whether the timeout has passed
passedTimeout :: State log -> IO Bool
passedTimeout st
  | Just s <- stTimeout st = do
      now <- round <$> getPOSIXTime
      return (now >= stStartTime st + s)
  | otherwise = return False

-- Compute the size of the next randomly generated value
computeSize :: State log -> Int
computeSize st
  | stNumPassed st `roundTo` stMaxGenSize st + stMaxGenSize st <= stMaxSuccess st
    || stNumPassed st >= stMaxSuccess st
    || stMaxSuccess st `mod` stMaxGenSize st == 0 =
    (stNumPassed st `mod` stMaxGenSize st + stNumDiscarded st `div` 10) `min` stMaxGenSize st
  | otherwise =
      ((stNumPassed st `mod` stMaxGenSize st) * stMaxGenSize st
       `div` (stMaxSuccess st `mod` stMaxGenSize st) + stNumDiscarded st `div` 10) `min` stMaxGenSize st

roundTo :: Integral a => a -> a -> a
roundTo n m = (n `div` m) * m

at0 :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
at0 _ s 0 0 = s
at0 f _ n d = f n d
