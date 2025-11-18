{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Edge-based bitmap trace store
module Test.Mutagen.Tracer.Store.Bitmap
  ( -- * Bitmap trace store
    TraceStore
  , newTraceStore
  , resetTraceStore
  , saveTrace
  , printTraceStore
  )
where

import Control.Monad (forM_, replicateM_)
import Data.Array.IO (IOUArray, getBounds, newArray, readArray, writeArray)
import Data.Foldable (foldrM)
import Test.Mutagen.Tracer.Store.API (TraceStoreImpl (..))
import Test.Mutagen.Tracer.Store.Types (TraceType (Bitmap))
import Test.Mutagen.Tracer.Trace (Trace (..))

{-------------------------------------------------------------------------------
-- * Bitmap trace store
-------------------------------------------------------------------------------}

instance TraceStoreImpl Bitmap where
  data TraceStore Bitmap = BitmapStore Int (IOUArray Int Bool)

  type SaveTraceResult Bitmap = Int

  newTraceStore n = do
    BitmapStore (n + 1) <$> newArray (0, (n + 1) * (n + 1)) False

  resetTraceStore (BitmapStore _ arr) = do
    (l, u) <- getBounds arr
    forM_ [l .. u] $ \i -> do
      writeArray arr i False

  saveTrace (Trace entries) (BitmapStore n arr) = do
    let edges = zip (0 : entries) entries
    let flipAndCount (i, j) acc = do
          let idx = i * n + j
          b <- readArray arr idx
          if b
            then return acc
            else writeArray arr idx True >> return (acc + 1)
    foldrM flipAndCount 0 edges

  printTraceStore (BitmapStore n arr) = do
    putStr "+" >> replicateM_ n (putStr "-") >> putStrLn "+"
    forM_ [0 .. n - 1] $ \i -> do
      putStr "|"
      forM_ [0 .. n - 1] $ \j -> do
        b <- readArray arr (i * n + j)
        if b then putStr "*" else putStr " "
      putStrLn "|"
    putStr "+" >> replicateM_ n (putStr "-") >> putStrLn "+"
