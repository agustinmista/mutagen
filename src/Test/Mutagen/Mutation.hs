{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | Mutations as transformations of values into mutants
module Test.Mutagen.Mutation
  ( -- * Mutable types
    Pos
  , Mutation
  , Mutable (..)
  , mutateEverywhere
  , wrap
  , node
  , invalidPosition
  , invalidPositionShow

    -- * Immutable wrapper
  , Immutable (..)

    -- * Mutation order
  , MutationOrder
  , preorder
  , postorder
  , levelorder
  )
where

import Data.Char (chr)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree (Tree (..), levels)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Test.Mutagen.Fragment.Store (sampleFragments)
import Test.Mutagen.Mutant (Mutant (..))
import Test.QuickCheck (Arbitrary (..), arbitrary)

{-------------------------------------------------------------------------------
-- * Mutable types
-------------------------------------------------------------------------------}

-- ** Types

-- | Breadcrumbs used to place mutations inside of values
type Pos = [Int]

-- | Mutations as transformations of values into mutants
type Mutation a = a -> [Mutant a]

-- ** Mutable class

-- | Mutable types
class (Typeable a) => Mutable a where
  -- | List all the possible positions whithin a value that accept mutations
  positions :: a -> Tree Pos
  positions _ = node []

  -- | Single value mutations
  -- The default value of the type to be used when mutating
  def :: a
  def = error "def: not defined"

  -- Top-level constructor mutations
  mutate :: Mutation a
  mutate = mempty

  -- Apply a top-level mutation inside a value
  inside :: Pos -> (forall x. (Mutable x) => Mutation x) -> Mutation a
  inside [] mut = mut
  inside pos _ = invalidPosition pos

-- | A mutation that acts everywhere inside a mutable value
mutateEverywhere :: (Mutable a) => Mutation a
mutateEverywhere a = topLevel <> nested
  where
    topLevel = mutate a
    nested = mconcat [inside pos mutate a | pos <- levelorder (positions a)]

-- ** Helpers for defining instances

-- | Wrap mutants with a constructor
wrap :: [Mutant a] -> (a -> b) -> [Mutant b]
wrap mutants wrapper = fmap (fmap wrapper) mutants

-- | Construct a position tree node from its indexed children
node :: [(Int, Tree Pos)] -> Tree Pos
node xs = Node [] (fmap (\(idx, children) -> fmap (idx :) children) xs)

-- | Report an invalid position error
invalidPosition :: Pos -> a
invalidPosition pos =
  error ("inside: invalid position: " <> show pos)

-- | Report an invalid position error, showing also the value being mutated
invalidPositionShow :: (Show a) => Pos -> a -> b
invalidPositionShow pos a =
  error ("inside: invalid position: " <> show pos <> "\nvalue: " <> show a)

{-------------------------------------------------------------------------------
-- * Immutable wrapper
-------------------------------------------------------------------------------}

-- | A mutable wrapper that produces no mutations.
--
-- This useful for constraning certain parts of a data structure to be
-- immutable while still fullfilling the 'Mutable' interface.
newtype Immutable a = Immutable {unImmutabe :: a}
  deriving (Eq, Ord, Read, Show)

instance (Arbitrary a) => Arbitrary (Immutable a) where
  arbitrary = Immutable <$> arbitrary

instance (Arbitrary a, Typeable a) => Mutable (Immutable a)

{-------------------------------------------------------------------------------
-- * Mutation order
-------------------------------------------------------------------------------}

-- | Order in which to traverse the mutation positions of a value
type MutationOrder = forall a. Tree a -> [a]

-- | Pre-order traversal
preorder :: MutationOrder
preorder t = squish t []
  where
    squish (Node x ts) xs = x : List.foldr squish xs ts

-- | Post-order traversal
postorder :: MutationOrder
postorder = squish []
  where
    squish xs (Node x ts) = x : List.foldl' squish xs ts

-- | Level-order traversal
levelorder :: MutationOrder
levelorder = concat . levels

{-------------------------------------------------------------------------------
-- * Instances
-------------------------------------------------------------------------------}

instance Mutable () where
  def = ()

instance Mutable Int where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Double where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Float where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Word8 where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Word16 where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Word32 where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Word64 where
  def = 0
  mutate n = [Rand arbitrary, Frag (sampleFragments n)]

instance Mutable Char where
  def = chr 0
  mutate c = [Rand arbitrary, Frag (sampleFragments c)]

instance Mutable Bool where
  def = False
  mutate b = [Pure (not b)]

instance (Mutable a) => Mutable (Maybe a) where
  positions Nothing = node []
  positions (Just a) = node [(0, positions a)]

  def = Nothing

  mutate Nothing = [Pure (Just def)]
  mutate (Just _) = [Pure Nothing]

  inside [] mut x = mut x
  inside (0 : ps) mut (Just a) = wrap (inside ps mut a) (\x -> Just x)
  inside pos _ _ = invalidPosition pos

instance (Mutable a, Mutable b) => Mutable (Either a b) where
  positions (Left a) = node [(0, positions a)]
  positions (Right b) = node [(0, positions b)]

  def = Left def

  mutate (Left _) = [Pure (Right def)]
  mutate (Right _) = [Pure (Left def)]

  inside [] mut x = mut x
  inside (0 : ps) mut (Left a) = wrap (inside ps mut a) (\x -> Left x)
  inside (0 : ps) mut (Right a) = wrap (inside ps mut a) (\x -> Right x)
  inside pos _ _ = invalidPosition pos

instance (Mutable a) => Mutable [a] where
  positions [] = node []
  positions (x : xs) = node [(0, positions x), (1, positions xs)]

  def = []

  -- NOTE: this instance additionally allows values to be duplicated
  mutate [] = [Pure [def]]
  mutate [x] = [Pure [], Pure [x, x]]
  mutate (x : xs) = [Pure [], Pure xs, Pure (x : x : xs)]

  inside [] mut xs = mut xs
  inside (0 : ps) mut (a : as) = wrap (inside ps mut a) (\x -> x : as)
  inside (1 : ps) mut (a : as) = wrap (inside ps mut as) (\xs -> a : xs)
  inside pos _ _ = invalidPosition pos

instance (Mutable v, Typeable k) => Mutable (Map k v) where
  positions m = node [(k, positions v) | (k, v) <- zip [0 ..] (Map.elems m)]

  def = Map.empty

  inside [] mut m = mut m
  inside (n : ps) mut m
    | n >= 0 && n < Map.size m =
        wrap
          (inside ps mut (snd (Map.elemAt n m)))
          (\x -> Map.updateAt (\_ _ -> Just x) n m)
    | otherwise =
        invalidPosition (n : ps)

-- ** Tuple instances

instance
  ( Mutable a
  , Mutable b
  )
  => Mutable (a, b)
  where
  positions (a, b) =
    node
      [ (0, positions a)
      , (1, positions b)
      ]

  def = (def, def)

  mutate (a, b) =
    [fmap (\x -> (x, b)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x)) gb | gb <- mutate b]

  inside pos mut x@(a, b) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  )
  => Mutable (a, b, c)
  where
  positions (a, b, c) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      ]

  def = (def, def, def)

  mutate (a, b, c) =
    [fmap (\x -> (x, b, c)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x)) gc | gc <- mutate c]

  inside pos mut x@(a, b, c) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  )
  => Mutable (a, b, c, d)
  where
  positions (a, b, c, d) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      ]

  def = (def, def, def, def)

  mutate (a, b, c, d) =
    [fmap (\x -> (x, b, c, d)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x)) gd | gd <- mutate d]

  inside pos mut x@(a, b, c, d) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  , Mutable e
  )
  => Mutable (a, b, c, d, e)
  where
  positions (a, b, c, d, e) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      , (4, positions e)
      ]

  def = (def, def, def, def, def)

  mutate (a, b, c, d, e) =
    [fmap (\x -> (x, b, c, d, e)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d, e)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d, e)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x, e)) gd | gd <- mutate d]
      <> [fmap (\x -> (a, b, c, d, x)) ge | ge <- mutate e]

  inside pos mut x@(a, b, c, d, e) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d, e))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d, e))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d, e))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d', e))
      (4 : ps) -> wrap (inside ps mut e) (\e' -> (a, b, c, d, e'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  , Mutable e
  , Mutable f
  )
  => Mutable (a, b, c, d, e, f)
  where
  positions (a, b, c, d, e, f) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      , (4, positions e)
      , (5, positions f)
      ]

  def = (def, def, def, def, def, def)

  mutate (a, b, c, d, e, f) =
    [fmap (\x -> (x, b, c, d, e, f)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d, e, f)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d, e, f)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x, e, f)) gd | gd <- mutate d]
      <> [fmap (\x -> (a, b, c, d, x, f)) ge | ge <- mutate e]
      <> [fmap (\x -> (a, b, c, d, e, x)) gf | gf <- mutate f]

  inside pos mut x@(a, b, c, d, e, f) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d, e, f))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d, e, f))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d, e, f))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d', e, f))
      (4 : ps) -> wrap (inside ps mut e) (\e' -> (a, b, c, d, e', f))
      (5 : ps) -> wrap (inside ps mut f) (\f' -> (a, b, c, d, e, f'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  , Mutable e
  , Mutable f
  , Mutable g
  )
  => Mutable (a, b, c, d, e, f, g)
  where
  positions (a, b, c, d, e, f, g) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      , (4, positions e)
      , (5, positions f)
      , (6, positions g)
      ]

  def = (def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g) =
    [fmap (\x -> (x, b, c, d, e, f, g)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d, e, f, g)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d, e, f, g)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x, e, f, g)) gd | gd <- mutate d]
      <> [fmap (\x -> (a, b, c, d, x, f, g)) ge | ge <- mutate e]
      <> [fmap (\x -> (a, b, c, d, e, x, g)) gf | gf <- mutate f]
      <> [fmap (\x -> (a, b, c, d, e, f, x)) gg | gg <- mutate g]

  inside pos mut x@(a, b, c, d, e, f, g) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d, e, f, g))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d, e, f, g))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d, e, f, g))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d', e, f, g))
      (4 : ps) -> wrap (inside ps mut e) (\e' -> (a, b, c, d, e', f, g))
      (5 : ps) -> wrap (inside ps mut f) (\f' -> (a, b, c, d, e, f', g))
      (6 : ps) -> wrap (inside ps mut g) (\g' -> (a, b, c, d, e, f, g'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  , Mutable e
  , Mutable f
  , Mutable g
  , Mutable h
  )
  => Mutable (a, b, c, d, e, f, g, h)
  where
  positions (a, b, c, d, e, f, g, h) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      , (4, positions e)
      , (5, positions f)
      , (6, positions g)
      , (7, positions h)
      ]

  def = (def, def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g, h) =
    [fmap (\x -> (x, b, c, d, e, f, g, h)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d, e, f, g, h)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d, e, f, g, h)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x, e, f, g, h)) gd | gd <- mutate d]
      <> [fmap (\x -> (a, b, c, d, x, f, g, h)) ge | ge <- mutate e]
      <> [fmap (\x -> (a, b, c, d, e, x, g, h)) gf | gf <- mutate f]
      <> [fmap (\x -> (a, b, c, d, e, f, x, h)) gg | gg <- mutate g]
      <> [fmap (\x -> (a, b, c, d, e, f, g, x)) gh | gh <- mutate h]

  inside pos mut x@(a, b, c, d, e, f, g, h) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d, e, f, g, h))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d, e, f, g, h))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d, e, f, g, h))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d', e, f, g, h))
      (4 : ps) -> wrap (inside ps mut e) (\e' -> (a, b, c, d, e', f, g, h))
      (5 : ps) -> wrap (inside ps mut f) (\f' -> (a, b, c, d, e, f', g, h))
      (6 : ps) -> wrap (inside ps mut g) (\g' -> (a, b, c, d, e, f, g', h))
      (7 : ps) -> wrap (inside ps mut h) (\h' -> (a, b, c, d, e, f, g, h'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  , Mutable e
  , Mutable f
  , Mutable g
  , Mutable h
  , Mutable i
  )
  => Mutable (a, b, c, d, e, f, g, h, i)
  where
  positions (a, b, c, d, e, f, g, h, i) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      , (4, positions e)
      , (5, positions f)
      , (6, positions g)
      , (7, positions h)
      , (8, positions i)
      ]

  def = (def, def, def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g, h, i) =
    [fmap (\x -> (x, b, c, d, e, f, g, h, i)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d, e, f, g, h, i)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d, e, f, g, h, i)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x, e, f, g, h, i)) gd | gd <- mutate d]
      <> [fmap (\x -> (a, b, c, d, x, f, g, h, i)) ge | ge <- mutate e]
      <> [fmap (\x -> (a, b, c, d, e, x, g, h, i)) gf | gf <- mutate f]
      <> [fmap (\x -> (a, b, c, d, e, f, x, h, i)) gg | gg <- mutate g]
      <> [fmap (\x -> (a, b, c, d, e, f, g, x, i)) gh | gh <- mutate h]
      <> [fmap (\x -> (a, b, c, d, e, f, g, h, x)) gi | gi <- mutate i]

  inside pos mut x@(a, b, c, d, e, f, g, h, i) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d, e, f, g, h, i))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d, e, f, g, h, i))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d, e, f, g, h, i))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d', e, f, g, h, i))
      (4 : ps) -> wrap (inside ps mut e) (\e' -> (a, b, c, d, e', f, g, h, i))
      (5 : ps) -> wrap (inside ps mut f) (\f' -> (a, b, c, d, e, f', g, h, i))
      (6 : ps) -> wrap (inside ps mut g) (\g' -> (a, b, c, d, e, f, g', h, i))
      (7 : ps) -> wrap (inside ps mut h) (\h' -> (a, b, c, d, e, f, g, h', i))
      (8 : ps) -> wrap (inside ps mut i) (\i' -> (a, b, c, d, e, f, g, h, i'))
      _ -> invalidPosition pos

instance
  ( Mutable a
  , Mutable b
  , Mutable c
  , Mutable d
  , Mutable e
  , Mutable f
  , Mutable g
  , Mutable h
  , Mutable i
  , Mutable j
  )
  => Mutable (a, b, c, d, e, f, g, h, i, j)
  where
  positions (a, b, c, d, e, f, g, h, i, j) =
    node
      [ (0, positions a)
      , (1, positions b)
      , (2, positions c)
      , (3, positions d)
      , (4, positions e)
      , (5, positions f)
      , (6, positions g)
      , (7, positions h)
      , (8, positions i)
      , (9, positions j)
      ]

  def = (def, def, def, def, def, def, def, def, def, def)

  mutate (a, b, c, d, e, f, g, h, i, j) =
    [fmap (\x -> (x, b, c, d, e, f, g, h, i, j)) ga | ga <- mutate a]
      <> [fmap (\x -> (a, x, c, d, e, f, g, h, i, j)) gb | gb <- mutate b]
      <> [fmap (\x -> (a, b, x, d, e, f, g, h, i, j)) gc | gc <- mutate c]
      <> [fmap (\x -> (a, b, c, x, e, f, g, h, i, j)) gd | gd <- mutate d]
      <> [fmap (\x -> (a, b, c, d, x, f, g, h, i, j)) ge | ge <- mutate e]
      <> [fmap (\x -> (a, b, c, d, e, x, g, h, i, j)) gf | gf <- mutate f]
      <> [fmap (\x -> (a, b, c, d, e, f, x, h, i, j)) gg | gg <- mutate g]
      <> [fmap (\x -> (a, b, c, d, e, f, g, x, i, j)) gh | gh <- mutate h]
      <> [fmap (\x -> (a, b, c, d, e, f, g, h, x, j)) gi | gi <- mutate i]
      <> [fmap (\x -> (a, b, c, d, e, f, g, h, i, x)) gj | gj <- mutate j]

  inside pos mut x@(a, b, c, d, e, f, g, h, i, j) =
    case pos of
      [] -> mut x
      (0 : ps) -> wrap (inside ps mut a) (\a' -> (a', b, c, d, e, f, g, h, i, j))
      (1 : ps) -> wrap (inside ps mut b) (\b' -> (a, b', c, d, e, f, g, h, i, j))
      (2 : ps) -> wrap (inside ps mut c) (\c' -> (a, b, c', d, e, f, g, h, i, j))
      (3 : ps) -> wrap (inside ps mut d) (\d' -> (a, b, c, d', e, f, g, h, i, j))
      (4 : ps) -> wrap (inside ps mut e) (\e' -> (a, b, c, d, e', f, g, h, i, j))
      (5 : ps) -> wrap (inside ps mut f) (\f' -> (a, b, c, d, e, f', g, h, i, j))
      (6 : ps) -> wrap (inside ps mut g) (\g' -> (a, b, c, d, e, f, g', h, i, j))
      (7 : ps) -> wrap (inside ps mut h) (\h' -> (a, b, c, d, e, f, g, h', i, j))
      (8 : ps) -> wrap (inside ps mut i) (\i' -> (a, b, c, d, e, f, g, h, i', j))
      (9 : ps) -> wrap (inside ps mut j) (\j' -> (a, b, c, d, e, f, g, h, i, j'))
      _ -> invalidPosition pos
