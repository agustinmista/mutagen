-- | Abstract test case mutants and their concretization.
module Test.Mutagen.Mutant
  ( -- * Abstract mutants
    Mutant (..)
  , MutantKind (..)

    -- * Concretized test cases
  , Concretized (..)
  , concretize

    -- * Re-exports
  , Gen
  , FragmentStore
  )
where

import Control.Monad (replicateM)
import Data.Typeable (Typeable)
import Test.Mutagen.Fragment.Store (FragmentStore)
import Test.QuickCheck (Gen, generate, resize)

{-------------------------------------------------------------------------------
-- * Abstract mutants
-------------------------------------------------------------------------------}

-- | Mutants representing possibly unrealized mutations over values of type @a@.
data Mutant a
  = -- | A pure mutation obtained after applying a deterministic transformation
    Pure a
  | -- | A random mutation obtained by sampling from a generator
    Rand (Gen a)
  | -- | A fragment-based mutation obtained by sampling from a fragment store
    Frag (FragmentStore -> Gen [a])

instance Show (Mutant a) where
  show (Pure _) = "Pure(..)"
  show (Rand _) = "Rand(..)"
  show (Frag _) = "Frag(..)"

instance Functor Mutant where
  fmap f (Pure mut) = Pure (f mut)
  fmap f (Rand gen) = Rand (fmap f gen)
  fmap f (Frag fun) = Frag (fmap (fmap (fmap f)) fun)

{-------------------------------------------------------------------------------
-- * Concretized test cases
-------------------------------------------------------------------------------}

-- | Kinds of concretized mutants.
data MutantKind = PureMutant | RandMutant | FragMutant
  deriving (Show)

-- | Values obtained by concretizing a mutant.
data Concretized a = Concretized MutantKind a
  deriving (Show)

-- | Turn an abstract mutant into a concrete set of values.
concretize
  :: (Typeable a)
  => (Int, Int)
  -- ^ Count and max generation size for random mutants
  -> (Int, FragmentStore)
  -- ^ Count and fragment store for fragment mutants
  -> Mutant a
  -- ^ Mutant to concretize
  -> IO [Concretized a]
concretize _ _ (Pure mut) = do
  fmap (Concretized PureMutant) <$> return [mut]
concretize (n, s) _ (Rand gen) = do
  fmap (Concretized RandMutant) <$> replicateM n (generate (resize s gen))
concretize _ (n, fs) (Frag fun) = do
  fmap (Concretized FragMutant) <$> fmap (take n) (generate (fun fs))
