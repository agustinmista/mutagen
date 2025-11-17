{-# LANGUAGE TypeFamilies #-}

module Test.Mutagen.Tracer.Store
  ( -- * Generic trace store interface
    TraceStore (..)
    -- * Trace store implementations
    , BitmapTraceStore
    , TreeTraceStore
  )
where

import Data.Kind
import Test.Mutagen.Tracer.Store.Bitmap (BitmapTraceStore)
import qualified Test.Mutagen.Tracer.Store.Bitmap as Bitmap
import Test.Mutagen.Tracer.Store.Tree (TreeTraceStore)
import qualified Test.Mutagen.Tracer.Store.Tree as Tree
import Test.Mutagen.Tracer.Trace

{-------------------------------------------------------------------------------
-- * Generic trace store interface
-------------------------------------------------------------------------------}

-- | Generic trace store interface
class TraceStore store where
  type SaveTraceOutput store :: Type
  -- ^ Output type of 'saveTrace' for the given store

  newTraceStore :: Int -> IO store
  -- ^ Create a new trace store for the given number of tracing nodes

  resetTraceStore :: store -> IO ()
  -- ^ Reset a trace store, deleting all saved traces but without deallocating it

  saveTrace :: Trace -> store -> IO (SaveTraceOutput store)
  -- ^ Save a trace into a trace store

  printTraceStore :: store -> IO ()
  -- ^ Print a trace store for debugging purposes

instance TraceStore BitmapTraceStore where
  type SaveTraceOutput BitmapTraceStore = Int
  newTraceStore = Bitmap.newTraceStore
  resetTraceStore = Bitmap.resetTraceStore
  saveTrace = Bitmap.saveTrace
  printTraceStore = Bitmap.printTraceStore

instance TraceStore TreeTraceStore where
  type SaveTraceOutput Tree.TreeTraceStore = (Int, Int)
  newTraceStore = const Tree.newTraceStore
  resetTraceStore = Tree.resetTraceStore
  saveTrace = Tree.saveTrace
  printTraceStore = Tree.printTraceStore
