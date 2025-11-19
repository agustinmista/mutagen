{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Mutagen.Fragment
  ( Fragment (..)
  , FragmentStore (..)
  , emptyFragmentStore
  , fragmentStoreSize
  , sampleFragments
  , storeFragments
  , printFragmentStore
  , FragmentTypeFilter (..)
  , Fragmentable (..)
  , singleton
  )
where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Word
import Test.QuickCheck

----------------------------------------
-- Test case fragments as existential typeable values

type IsFragment a = (Typeable a, Ord a, Show a)

data Fragment = forall a. (IsFragment a) => Fragment a

instance Eq Fragment where
  Fragment f1 == Fragment f2 =
    case cast f2 of
      Just f2' -> f1 == f2'
      Nothing -> False

instance Ord Fragment where
  compare (Fragment f1) (Fragment f2) =
    case cast f2 of
      Just f2' -> compare f1 f2'
      Nothing -> LT

instance Show Fragment where
  show (Fragment a) = "Fragment(" <> show a <> ")"

----------------------------------------
-- Storing fragments based on their type

newtype FragmentStore = FragmentStore (Map TypeRep (Set Fragment))

fragmentStoreSize :: FragmentStore -> [(TypeRep, Int)]
fragmentStoreSize (FragmentStore fs) =
  [ (tyRep, Set.size frags)
  | (tyRep, frags) <- Map.toList fs
  ]

instance Semigroup FragmentStore where
  FragmentStore fs1 <> FragmentStore fs2 =
    FragmentStore (Map.unionWith Set.union fs1 fs2)

instance Monoid FragmentStore where
  mempty = emptyFragmentStore

printFragmentStore :: FragmentStore -> IO ()
printFragmentStore (FragmentStore fs) = do
  forM_ (Map.assocs fs) $ \(tyRep, frags) -> do
    putStrLn ("TypeRep: " <> show tyRep)
    forM_ frags $ \frag -> do
      putStrLn ("* " <> show frag)

emptyFragmentStore :: FragmentStore
emptyFragmentStore = FragmentStore mempty

insertFragment
  :: FragmentTypeFilter
  -> TypeRep
  -> Fragment
  -> FragmentStore
  -> FragmentStore
insertFragment f tyRep fr (FragmentStore fs)
  | isFragmentTypeAllowed f tyRep =
      FragmentStore (Map.insertWith Set.union tyRep (Set.singleton fr) fs)
  | otherwise =
      FragmentStore fs

collectFragments
  :: (Fragmentable a)
  => FragmentTypeFilter
  -> a
  -> FragmentStore
collectFragments f a = foldr (uncurry (insertFragment f)) emptyFragmentStore fts
  where
    fts = Set.map (\(Fragment x) -> (typeOf x, Fragment x)) (fragmentize a)

storeFragments
  :: (Fragmentable a)
  => FragmentTypeFilter
  -> a
  -> FragmentStore
  -> FragmentStore
storeFragments f a fs = fs <> collectFragments f a

sampleFragments
  :: (Typeable a)
  => a
  -> FragmentStore
  -> Gen [a]
sampleFragments a (FragmentStore fs) = do
  case Map.lookup (typeOf a) fs of
    Nothing ->
      return []
    Just frags ->
      mapMaybe (\(Fragment a') -> cast a') <$> shuffle (Set.toList frags)

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

----------------------------------------
-- Fragmentizing values

class (IsFragment a) => Fragmentable a where
  fragmentize :: a -> Set Fragment
  fragmentize = singleton

-- Helpers

singleton :: (Fragmentable a) => a -> Set Fragment
singleton = Set.singleton . Fragment

----------------------------------------
-- Fragmentable instances
----------------------------------------

instance Fragmentable ()

instance Fragmentable Int

instance Fragmentable Double

instance Fragmentable Float

instance Fragmentable Word8

instance Fragmentable Word16

instance Fragmentable Word32

instance Fragmentable Word64

instance Fragmentable Char

instance Fragmentable Bool

instance (Fragmentable a) => Fragmentable (Maybe a) where
  fragmentize x =
    case x of
      Nothing -> singleton x
      Just v1 -> singleton x <> fragmentize v1

instance (Fragmentable a, Fragmentable b) => Fragmentable (Either a b) where
  fragmentize (Left x) = singleton @(Either a b) (Left x) <> fragmentize x
  fragmentize (Right x) = singleton @(Either a b) (Right x) <> fragmentize x

instance (Fragmentable a) => Fragmentable [a] where
  fragmentize [] = singleton @[a] []
  fragmentize (x : xs) = singleton @[a] (x : xs) <> fragmentize x <> fragmentize xs

instance (Fragmentable k, Fragmentable v) => Fragmentable (Map k v) where
  fragmentize m = mconcat [fragmentize k <> fragmentize v | (k, v) <- Map.toList m]

-- Tuple instances

instance (Fragmentable a, Fragmentable b) => Fragmentable (a, b) where
  fragmentize (a, b) = singleton @(a, b) (a, b) <> fragmentize a <> fragmentize b

instance (Fragmentable a, Fragmentable b, Fragmentable c) => Fragmentable (a, b, c) where
  fragmentize (a, b, c) = singleton @(a, b, c) (a, b, c) <> fragmentize a <> fragmentize b <> fragmentize c

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d) => Fragmentable (a, b, c, d) where
  fragmentize (a, b, c, d) = singleton @(a, b, c, d) (a, b, c, d) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e) => Fragmentable (a, b, c, d, e) where
  fragmentize (a, b, c, d, e) = singleton @(a, b, c, d, e) (a, b, c, d, e) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f) => Fragmentable (a, b, c, d, e, f) where
  fragmentize (a, b, c, d, e, f) = singleton @(a, b, c, d, e, f) (a, b, c, d, e, f) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g) => Fragmentable (a, b, c, d, e, f, g) where
  fragmentize (a, b, c, d, e, f, g) = singleton @(a, b, c, d, e, f, g) (a, b, c, d, e, f, g) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g, Fragmentable h) => Fragmentable (a, b, c, d, e, f, g, h) where
  fragmentize (a, b, c, d, e, f, g, h) = singleton @(a, b, c, d, e, f, g, h) (a, b, c, d, e, f, g, h) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g <> fragmentize h

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g, Fragmentable h, Fragmentable i) => Fragmentable (a, b, c, d, e, f, g, h, i) where
  fragmentize (a, b, c, d, e, f, g, h, i) = singleton @(a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g, h, i) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g <> fragmentize h <> fragmentize i

instance (Fragmentable a, Fragmentable b, Fragmentable c, Fragmentable d, Fragmentable e, Fragmentable f, Fragmentable g, Fragmentable h, Fragmentable i, Fragmentable j) => Fragmentable (a, b, c, d, e, f, g, h, i, j) where
  fragmentize (a, b, c, d, e, f, g, h, i, j) = singleton @(a, b, c, d, e, f, g, h, i, j) (a, b, c, d, e, f, g, h, i, j) <> fragmentize a <> fragmentize b <> fragmentize c <> fragmentize d <> fragmentize e <> fragmentize f <> fragmentize g <> fragmentize h <> fragmentize i <> fragmentize j
