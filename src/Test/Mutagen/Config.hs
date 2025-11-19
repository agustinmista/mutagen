{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Configuration options for Mutagen
module Test.Mutagen.Config
  ( -- * Configuration options
    Config (..)
  , defaultConfig

    -- * Helpers
  , allow
  , allow'
  , deny
  , deny'
  , example
  , DebugMode (..)

    -- * Re-exports
  , FragmentTypeFilter
  , TraceType (..)
  )
where

import qualified Data.Set as Set
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Test.Mutagen.Fragment (FragmentTypeFilter (..))
import Test.Mutagen.Mutation (MutationOrder, levelorder)
import Test.Mutagen.Property (Args (..), IsArgs)
import Test.Mutagen.Tracer.Store (TraceType (..))

{-------------------------------------------------------------------------------
-- * Configuration options
-------------------------------------------------------------------------------}

-- | Configuration options for Mutagen
data Config
  = Config
  { maxSuccess :: Int
  -- ^ Max number of passed tests.
  , maxDiscardRatio :: Int
  -- ^ Max discard ratio.
  , timeout :: Maybe Integer
  -- ^ Campaign time budget in seconds (has precedence over maxSuccess).
  , expect :: Bool
  -- ^ Whether the property is expected to hold (True) or to fail (False).
  , maxGenSize :: Int
  -- ^ Max generation size passed to a generator. It uses the same formula for
  -- computing sizes as vanilla QuickCheck when in generation mode. Random
  -- mutations are generated using the maximum size.
  , randomMutations :: Int
  -- ^ Number of times to sample the generator associated to a random mutant.
  -- It can be automatically increased over time if `autoResetAfter` is not set
  -- to `Nothing`.
  , mutationLimit :: Maybe Int
  -- ^ The maximum number of ancestors a test case can have before being
  -- discarded. Useful to avoid mutating recursive structures indefinetely.
  -- Defaults to `maxGenSize` if set to `Nothing`.
  , autoResetAfter :: Maybe Int
  -- ^ Reset the global trace log if no interesting test case is found after a
  -- certain number of tests. If not set to `Nothing`, this will duplicate the
  -- current limit on every reset. Additionally, it also duplicates the
  -- `randomMutations` parameter.
  , useLazyPrunning :: Bool
  -- ^ Use lazy prunning to avoid mutating unevaluated subexpressions. The
  -- target mutable subexpressions are ordered by last evaluated first.
  , mutationOrder :: MutationOrder
  -- ^ If `useLazyPrunning` is set to `False`, *every* subexpression of an
  -- interesting test case is mutated regardless whether it was evaluated or
  -- not. These subexpressions are ordered using a generic tree traversal order
  -- (level order by default). Options are: `levelorder`, `preorder`, and
  -- `postorder`.
  , useFragments :: Bool
  -- ^ Explode the interesting test cases found during the test loop into typed
  -- fragments. These fragments can be used to concretize fragment mutants.
  , randomFragments :: Int
  -- ^ The amount of fragments sampled from the global fragment store when
  -- concretizing a fragment mutant. Can return less than `randomFragments` test
  -- cases if there are not enough fragments of the type of the target
  -- subexpression to sample from.
  , filterFragments :: FragmentTypeFilter
  -- ^ Filter to allow or deny values of certain types from being saved in the
  -- fragment store.
  , examples :: [Args]
  -- ^ Initial inputs examples used to populate the global fragment store before
  -- the testing loop starts.
  , traceType :: TraceType
  -- ^ The tracing log mechanism. Either `Tree` or `Bitmap`. `Tree` uses
  -- prefix-based traces (quite expensive but more precise). `Bitmap` uses
  -- edge-based traces (cheaper but less precise).
  , maxTraceLength :: Maybe Int
  -- ^ The maximim trace length to consider. Useful in conjunction with the
  -- `Tree` `traceMethod` when testing lengthy properties.
  , chatty :: Bool
  -- ^ Print extra info.
  , debug :: DebugMode
  -- ^ Whether to enable interactive debugging mode.
  }

-- | Default configuration options for Mutagen
defaultConfig :: Config
defaultConfig =
  Config
    { maxSuccess = 1000000
    , maxDiscardRatio = 1000
    , timeout = Nothing
    , expect = True
    , maxGenSize = 10
    , randomMutations = 1
    , mutationLimit = Nothing
    , autoResetAfter = Just 1000
    , useLazyPrunning = False
    , mutationOrder = levelorder
    , useFragments = False
    , randomFragments = 10
    , filterFragments = mempty
    , examples = []
    , traceType = Bitmap
    , maxTraceLength = Nothing
    , chatty = False
    , debug = NoDebug
    }

{-------------------------------------------------------------------------------
-- * Helpers
-------------------------------------------------------------------------------}

-- | Allow a type to be saved in the fragment store
allow :: forall a. (Typeable a) => FragmentTypeFilter
allow = FragmentTypeFilter (Set.singleton (typeRep (Proxy @a))) mempty

-- | Like 'allow' but taking a 'Proxy' argument
allow' :: forall a. (Typeable a) => Proxy a -> FragmentTypeFilter
allow' _ = allow @a

-- | Deny a type from being saved in the fragment store
deny :: forall a. (Typeable a) => FragmentTypeFilter
deny = FragmentTypeFilter mempty (Set.singleton (typeRep (Proxy @a)))

-- | Like 'deny' but taking a 'Proxy' argument
deny' :: forall a. (Typeable a) => Proxy a -> FragmentTypeFilter
deny' _ = deny @a

-- | Helper to create an example input of any supported argument type
example :: forall a. (IsArgs a) => a -> Args
example = Args

-- | Debugging mode
--
-- Allows stopping the loop between test cases to inspect the internal state.
data DebugMode
  = -- | Run normally without stopping between tests
    NoDebug
  | -- | Stop after every passed test case
    StopOnPassed
  | -- | Stop after every test case (passed or discarded)
    AlwaysStop
  deriving (Eq, Show)
