{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test case fragments and fragment stores
module Test.Mutagen.Fragment
  ( -- * Fragments and Fragmentable class
    Fragment (..)
  , Fragmentable (..)
  , singleton
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable, cast)
import Data.Word (Word16, Word32, Word64, Word8)

{-------------------------------------------------------------------------------
-- * Fragments and Fragmentable class
-------------------------------------------------------------------------------}

-- | Fragment type class constraint
type IsFragment a = (Typeable a, Ord a, Show a)

-- | A test case fragment hidden behind an existential
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

-- | Lift a item into a singleton fragment set
singleton :: (IsFragment a) => a -> Set Fragment
singleton = Set.singleton . Fragment

-- ** Fragmentable class

-- | Types that can be fragmented into smaller pieces
class (IsFragment a) => Fragmentable a where
  -- | Extract fragments from a value
  fragmentize :: a -> Set Fragment
  fragmentize = singleton

{-------------------------------------------------------------------------------
-- * Fragmentable instances
-------------------------------------------------------------------------------}

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
  fragmentize (Left x) =
    singleton @(Either a b) (Left x)
      <> fragmentize x
  fragmentize (Right x) =
    singleton @(Either a b) (Right x)
      <> fragmentize x

instance (Fragmentable a) => Fragmentable [a] where
  fragmentize [] = singleton @[a] []
  fragmentize (x : xs) =
    singleton @[a] (x : xs)
      <> fragmentize x
      <> fragmentize xs

instance (Fragmentable k, Fragmentable v) => Fragmentable (Map k v) where
  fragmentize m =
    mconcat
      [ fragmentize k <> fragmentize v
      | (k, v) <- Map.toList m
      ]

-- Tuple instances

instance
  ( Fragmentable a
  , Fragmentable b
  )
  => Fragmentable (a, b)
  where
  fragmentize (a, b) =
    singleton (a, b)
      <> fragmentize a
      <> fragmentize b

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  )
  => Fragmentable (a, b, c)
  where
  fragmentize (a, b, c) =
    singleton (a, b, c)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  )
  => Fragmentable (a, b, c, d)
  where
  fragmentize (a, b, c, d) =
    singleton (a, b, c, d)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  , Fragmentable e
  )
  => Fragmentable (a, b, c, d, e)
  where
  fragmentize (a, b, c, d, e) =
    singleton (a, b, c, d, e)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d
      <> fragmentize e

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  , Fragmentable e
  , Fragmentable f
  )
  => Fragmentable (a, b, c, d, e, f)
  where
  fragmentize (a, b, c, d, e, f) =
    singleton (a, b, c, d, e, f)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d
      <> fragmentize e
      <> fragmentize f

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  , Fragmentable e
  , Fragmentable f
  , Fragmentable g
  )
  => Fragmentable (a, b, c, d, e, f, g)
  where
  fragmentize (a, b, c, d, e, f, g) =
    singleton (a, b, c, d, e, f, g)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d
      <> fragmentize e
      <> fragmentize f
      <> fragmentize g

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  , Fragmentable e
  , Fragmentable f
  , Fragmentable g
  , Fragmentable h
  )
  => Fragmentable (a, b, c, d, e, f, g, h)
  where
  fragmentize (a, b, c, d, e, f, g, h) =
    singleton (a, b, c, d, e, f, g, h)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d
      <> fragmentize e
      <> fragmentize f
      <> fragmentize g
      <> fragmentize h

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  , Fragmentable e
  , Fragmentable f
  , Fragmentable g
  , Fragmentable h
  , Fragmentable i
  )
  => Fragmentable (a, b, c, d, e, f, g, h, i)
  where
  fragmentize (a, b, c, d, e, f, g, h, i) =
    singleton (a, b, c, d, e, f, g, h, i)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d
      <> fragmentize e
      <> fragmentize f
      <> fragmentize g
      <> fragmentize h
      <> fragmentize i

instance
  ( Fragmentable a
  , Fragmentable b
  , Fragmentable c
  , Fragmentable d
  , Fragmentable e
  , Fragmentable f
  , Fragmentable g
  , Fragmentable h
  , Fragmentable i
  , Fragmentable j
  )
  => Fragmentable (a, b, c, d, e, f, g, h, i, j)
  where
  fragmentize (a, b, c, d, e, f, g, h, i, j) =
    singleton (a, b, c, d, e, f, g, h, i, j)
      <> fragmentize a
      <> fragmentize b
      <> fragmentize c
      <> fragmentize d
      <> fragmentize e
      <> fragmentize f
      <> fragmentize g
      <> fragmentize h
      <> fragmentize i
      <> fragmentize j
