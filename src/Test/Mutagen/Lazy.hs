-- | Tracking lazy evaluation of expressions
module Test.Mutagen.Lazy
  ( -- * Lazy evaluation tracking interface
    __evaluated__
  , resetPosRef
  , readPosRef

    -- * Lazy type class
  , Lazy (..)
  )
where

import Data.IORef
-- For providing some default Lazy instances

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import System.IO.Unsafe
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
  lazy :: a -> a
  lazy = lazyNode []

  lazyNode :: Pos -> a -> a

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
