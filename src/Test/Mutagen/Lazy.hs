{-# LANGUAGE BangPatterns #-}

-- | Tracking lazy evaluation of expressions
module Test.Mutagen.Lazy
  ( -- * Lazy evaluation tracking interface
    __evaluated__

    -- * Lazy type class
  , Lazy (..)
  , withLazy
  , withLazyIO
  )
where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16, Word32, Word64, Word8)
import System.IO.Unsafe (unsafePerformIO)
import Test.Mutagen.Mutation (Pos)

{-------------------------------------------------------------------------------
-- * Lazy evaluation tracking interface
-------------------------------------------------------------------------------}

-- | Injectable function to mark the evaluation an expression at some possition
__evaluated__ :: Pos -> a -> a
__evaluated__ pos expr =
  unsafePerformIO $ do
    addEvaluatedPos pos
    return expr
{-# INLINE __evaluated__ #-}

-- | Global IORef to store evaluated positions
posRef :: IORef [Pos]
posRef = unsafePerformIO (newIORef [])
{-# NOINLINE posRef #-}

-- | Add evaluated position to the global IORef
addEvaluatedPos :: Pos -> IO ()
addEvaluatedPos pos = modifyIORef' posRef (reverse pos :)

-- | Reset the global IORef of evaluated positions
resetPosRef :: IO ()
resetPosRef = modifyIORef' posRef (const [])

-- | Read the global IORef of evaluated positions
readPosRef :: IO [Pos]
readPosRef = reverse <$> readIORef posRef

{-------------------------------------------------------------------------------
-- * Lazy type class
-------------------------------------------------------------------------------}

-- | Class for types that can track lazy evaluation of their subexpressions
class Lazy a where
  -- | Wrap an entire value (i.e., at every subexpression) with calls to
  -- '__evaluated__' with their corresponding positions.
  --
  -- This is a convenience function defined as:
  --
  -- @
  -- lazy x  = lazyNode [] x
  -- @
  --
  -- And you usually want to define 'lazyNode' instead.
  lazy :: a -> a
  lazy = lazyNode []

  -- | Wrap a value at a given position with calls to '__evaluated__'.
  --
  -- You can use 'withLazy' to test which subexpressions are evaluated by a
  -- given function. For example:
  --
  -- >>> let a = Right (undefined, Just 42) :: Either Bool (String, Maybe Int)
  -- >>> withLazy (\x -> case x of Right (_, Just _) -> True; _ -> False) a
  -- ([[],[0],[0,1]],True)
  --
  -- Which indicates that the function evaluated:
  --
  -- * [] -> root node (Right)
  -- * [0] -> Right's 0th child (the tuple)
  -- * [0,1] -> the tuple's 1st child (Just)
  --
  -- While not evaluating neihter the @undefined@ nor the @42@.
  lazyNode :: Pos -> a -> a

  {-# MINIMAL lazyNode #-}

-- | Find which subexpressions of an input value does a function evaluate when
-- forcing its result to weak head normal form.
withLazy :: (Lazy a) => (a -> b) -> a -> IO ([Pos], b)
withLazy f a = do
  resetPosRef
  let !b = f (lazy a)
  ps <- readPosRef
  return (ps, b)

-- | Like 'withLazy', but for functions that already run on IO
withLazyIO :: (Lazy a) => (a -> IO b) -> a -> IO ([Pos], b)
withLazyIO f a = do
  resetPosRef
  !b <- f (lazy a)
  ps <- readPosRef
  return (ps, b)

-- ** Lazy instances

instance Lazy () where
  lazyNode = __evaluated__

instance Lazy Int where
  lazyNode = __evaluated__

instance Lazy Double where
  lazyNode = __evaluated__

instance Lazy Float where
  lazyNode = __evaluated__

instance Lazy Word8 where
  lazyNode = __evaluated__

instance Lazy Word16 where
  lazyNode = __evaluated__

instance Lazy Word32 where
  lazyNode = __evaluated__

instance Lazy Word64 where
  lazyNode = __evaluated__

instance Lazy Char where
  lazyNode = __evaluated__

instance Lazy Bool where
  lazyNode = __evaluated__

instance (Lazy a) => Lazy (Maybe a) where
  lazyNode pre Nothing =
    __evaluated__
      pre
      Nothing
  lazyNode pre (Just a) =
    __evaluated__ pre
      $ Just (lazyNode (0 : pre) a)

instance (Lazy a, Lazy b) => Lazy (Either a b) where
  lazyNode pre (Left x) =
    __evaluated__ pre
      $ Left (lazyNode (0 : pre) x)
  lazyNode pre (Right x) =
    __evaluated__ pre
      $ Right (lazyNode (0 : pre) x)

instance (Lazy a) => Lazy [a] where
  lazyNode pre [] =
    __evaluated__
      pre
      []
  lazyNode pre (x : xs) =
    __evaluated__
      pre
      (lazyNode (0 : pre) x : lazyNode (1 : pre) xs)

instance (Lazy v) => Lazy (Map k v) where
  lazyNode pre m =
    snd (Map.mapAccum f 0 m)
    where
      f c v = (c + 1, __evaluated__ pre (lazyNode (c : pre) v))

-- Tuple instances

instance
  (Lazy a, Lazy b)
  => Lazy (a, b)
  where
  lazyNode pre (a, b) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      )

instance
  (Lazy a, Lazy b, Lazy c)
  => Lazy (a, b, c)
  where
  lazyNode pre (a, b, c) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d)
  => Lazy (a, b, c, d)
  where
  lazyNode pre (a, b, c, d) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e)
  => Lazy (a, b, c, d, e)
  where
  lazyNode pre (a, b, c, d, e) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      , lazyNode (4 : pre) e
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f)
  => Lazy (a, b, c, d, e, f)
  where
  lazyNode pre (a, b, c, d, e, f) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      , lazyNode (4 : pre) e
      , lazyNode (5 : pre) f
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g)
  => Lazy (a, b, c, d, e, f, g)
  where
  lazyNode pre (a, b, c, d, e, f, g) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      , lazyNode (4 : pre) e
      , lazyNode (5 : pre) f
      , lazyNode (6 : pre) g
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h)
  => Lazy (a, b, c, d, e, f, g, h)
  where
  lazyNode pre (a, b, c, d, e, f, g, h) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      , lazyNode (4 : pre) e
      , lazyNode (5 : pre) f
      , lazyNode (6 : pre) g
      , lazyNode (7 : pre) h
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h, Lazy i)
  => Lazy (a, b, c, d, e, f, g, h, i)
  where
  lazyNode pre (a, b, c, d, e, f, g, h, i) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      , lazyNode (4 : pre) e
      , lazyNode (5 : pre) f
      , lazyNode (6 : pre) g
      , lazyNode (7 : pre) h
      , lazyNode (8 : pre) i
      )

instance
  (Lazy a, Lazy b, Lazy c, Lazy d, Lazy e, Lazy f, Lazy g, Lazy h, Lazy i, Lazy j)
  => Lazy (a, b, c, d, e, f, g, h, i, j)
  where
  lazyNode pre (a, b, c, d, e, f, g, h, i, j) =
    __evaluated__
      pre
      ( lazyNode (0 : pre) a
      , lazyNode (1 : pre) b
      , lazyNode (2 : pre) c
      , lazyNode (3 : pre) d
      , lazyNode (4 : pre) e
      , lazyNode (5 : pre) f
      , lazyNode (6 : pre) g
      , lazyNode (7 : pre) h
      , lazyNode (8 : pre) i
      , lazyNode (9 : pre) j
      )
