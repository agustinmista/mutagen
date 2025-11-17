{-# LANGUAGE FlexibleContexts #-}

-- | Edge-based bitmap trace store
module Test.Mutagen.Tracer.Store.Bitmap
  ( -- * Bitmap trace store
    BitmapTraceStore
  , newTraceStore
  , resetTraceStore
  , saveTrace
  , printTraceStore
  )
where

import Control.Monad (forM_, replicateM_)
import Data.Array.IO (IOUArray, getBounds, newArray, readArray, writeArray)
import Data.Foldable (foldrM)
import Test.Mutagen.Tracer.Trace (Trace (..))

{-------------------------------------------------------------------------------
-- * Bitmap trace store
-------------------------------------------------------------------------------}

-- | Edge-based bitmap trace store
data BitmapTraceStore
  = BitmapTraceStore
      Int
      -- ^ Size (number of tracing nodes)
      (IOUArray Int Bool)
      -- ^ Bitmap array

-- | Create an empty trace store
newTraceStore :: Int -> IO BitmapTraceStore
newTraceStore n =
  BitmapTraceStore (n + 1) <$> newArray (0, (n + 1) * (n + 1)) False

-- | Reset a trace store
resetTraceStore :: BitmapTraceStore -> IO ()
resetTraceStore (BitmapTraceStore _ arr) = do
  (l, u) <- getBounds arr
  forM_ [l .. u] $ \i -> do
    writeArray arr i False

-- | Save a trace into a trace store
saveTrace :: Trace -> BitmapTraceStore -> IO Int
saveTrace (Trace entries) (BitmapTraceStore n arr) = do
  let edges = zip (0 : entries) entries
  let flipAndCount (i, j) acc = do
        let idx = i * n + j
        b <- readArray arr idx
        if b
          then return acc
          else writeArray arr idx True >> return (acc + 1)
  foldrM flipAndCount 0 edges

-- | Print a trace store
printTraceStore :: BitmapTraceStore -> IO ()
printTraceStore (BitmapTraceStore n arr) = do
  putStr "+" >> replicateM_ n (putStr "-") >> putStrLn "+"
  forM_ [0 .. n - 1] $ \i -> do
    putStr "|"
    forM_ [0 .. n - 1] $ \j -> do
      b <- readArray arr (i * n + j)
      if b then putStr "*" else putStr " "
    putStrLn "|"
  putStr "+" >> replicateM_ n (putStr "-") >> putStrLn "+"
