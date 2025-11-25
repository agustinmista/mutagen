{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic trace store interface.
module Test.Mutagen.Tracer.Store.API
  ( -- * Generic trace store interface
    TraceStoreImpl (..)
  )
where

import Test.Mutagen.Tracer.Store.Types (TraceBackend)
import Test.Mutagen.Tracer.Trace (Trace)

{-------------------------------------------------------------------------------
-- * Generic trace store interface
-------------------------------------------------------------------------------}

-- | Generic trace store interface.
--
-- NOTE: in this context, "store" refers to a data structure that one can "save"
-- traces into, while getting some useful kind of signal in return (i.e. a
-- 'SaveTraceResult'). It does not refer to a persistence mechanism that would
-- allow one to get a trace back in any way. In practice, most implementations
-- would be lossy to some extent to keep memory usage low.
class TraceStoreImpl (trace :: TraceBackend) where
  data TraceStore trace
  -- ^ Concrete trace store type for the given tracing backend.

  type SaveTraceResult trace
  -- ^ Output type of 'saveTrace' for the given store.

  newTraceStore :: Int -> IO (TraceStore trace)
  -- ^ Create a new trace store for the given number of tracing nodes.

  resetTraceStore :: TraceStore trace -> IO ()
  -- ^ Reset a trace store, deleting all saved traces without deallocating it.

  saveTrace :: Trace -> TraceStore trace -> IO (SaveTraceResult trace)
  -- ^ Save a trace into a trace store.

  printTraceStore :: TraceStore trace -> IO ()
  -- ^ Print a trace store for debugging purposes.
