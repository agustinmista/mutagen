{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic trace store interface
module Test.Mutagen.Tracer.Store.API
  ( -- * Generic trace store interface
    TraceStoreImpl (..)
  )
where

import Test.Mutagen.Tracer.Store.Types (TraceType)
import Test.Mutagen.Tracer.Trace (Trace)

{-------------------------------------------------------------------------------
-- * Generic trace store interface
-------------------------------------------------------------------------------}

-- | Generic trace store interface
class TraceStoreImpl (trace :: TraceType) where
  data TraceStore trace
  -- ^ Concrete trace store type for the given tracing method

  type SaveTraceResult trace
  -- ^ Output type of 'saveTrace' for the given store

  newTraceStore :: Int -> IO (TraceStore trace)
  -- ^ Create a new trace store for the given number of tracing nodes

  resetTraceStore :: TraceStore trace -> IO ()
  -- ^ Reset a trace store, deleting all saved traces without deallocating it

  saveTrace :: Trace -> TraceStore trace -> IO (SaveTraceResult trace)
  -- ^ Save a trace into a trace store

  printTraceStore :: TraceStore trace -> IO ()
  -- ^ Print a trace store for debugging purposes
