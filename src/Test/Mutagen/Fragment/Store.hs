-- | Type-indexed fragment store for collecting and sampling fragments from
module Test.Mutagen.Fragment.Store
  ( -- * Type-indexed fragment store
    FragmentStore (..)
  , emptyFragmentStore
  , fragmentStoreSize
  , storeFragments
  , sampleFragments
  , printFragmentStore
  , FragmentTypeFilter (..)
  , isFragmentTypeAllowed
  )
where

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, cast, typeOf)
import Test.Mutagen.Fragment (Fragment (..), Fragmentable (..), singleton)
import Test.QuickCheck (Gen, shuffle)

{-------------------------------------------------------------------------------
-- * Type-indexed fragment store
-------------------------------------------------------------------------------}

-- | A collection of fragments indexed by their type representation
newtype FragmentStore = FragmentStore (Map TypeRep (Set Fragment))

instance Semigroup FragmentStore where
  FragmentStore fs1 <> FragmentStore fs2 =
    FragmentStore (Map.unionWith Set.union fs1 fs2)

instance Monoid FragmentStore where
  mempty = emptyFragmentStore

-- | An empty fragment store
emptyFragmentStore :: FragmentStore
emptyFragmentStore = FragmentStore mempty

-- | Get the number of fragments stored for each type
fragmentStoreSize :: FragmentStore -> [(TypeRep, Int)]
fragmentStoreSize (FragmentStore fs) =
  [ (tyRep, Set.size frags)
  | (tyRep, frags) <- Map.toList fs
  ]

-- | Store fragments from a value into the fragment store
storeFragments
  :: (Fragmentable a)
  => FragmentTypeFilter
  -> a
  -> FragmentStore
  -> FragmentStore
storeFragments typeFilter a (FragmentStore store) =
  FragmentStore (Map.unionWith Set.union store (collect a))
  where
    collect = foldr insertIfAllowed Map.empty . fragmentize

    insertIfAllowed (Fragment a') store'
      | isFragmentTypeAllowed typeFilter (typeOf a') =
          Map.insertWith Set.union (typeOf a') (singleton a') store'
      | otherwise =
          store'

-- | Sample fragments of the same type as the given value from a fragment store
sampleFragments
  :: (Typeable a)
  => a
  -> FragmentStore
  -> Gen [a]
sampleFragments a (FragmentStore store) = do
  case Map.lookup (typeOf a) store of
    Nothing ->
      return []
    Just frags ->
      mapMaybe (\(Fragment a') -> cast a') <$> shuffle (Set.toList frags)

-- | Print the contents of a fragment store for debugging purposes
printFragmentStore :: FragmentStore -> IO ()
printFragmentStore (FragmentStore fs) = do
  forM_ (Map.assocs fs) $ \(tyRep, frags) -> do
    putStrLn ("TypeRep: " <> show tyRep)
    forM_ frags $ \frag -> do
      putStrLn ("* " <> show frag)

-- ** Fragment type filters

-- | Fragment type allow and deny lists
data FragmentTypeFilter = FragmentTypeFilter
  { allowList :: Set TypeRep
  -- ^ List of allowed fragment types
  , denyList :: Set TypeRep
  -- ^ List of denied fragment types
  }
  deriving (Eq, Show)

instance Semigroup FragmentTypeFilter where
  (FragmentTypeFilter a1 d1) <> (FragmentTypeFilter a2 d2) =
    FragmentTypeFilter (a1 <> a2) (d1 <> d2)

instance Monoid FragmentTypeFilter where
  mempty = FragmentTypeFilter mempty mempty

-- | Check if a type is allowed by the fragment type filter
isFragmentTypeAllowed :: FragmentTypeFilter -> TypeRep -> Bool
isFragmentTypeAllowed (FragmentTypeFilter allow deny) tr =
  (tr `Set.member` allow)
    && not (tr `Set.member` deny)
