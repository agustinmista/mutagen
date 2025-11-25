{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Trace store implementations based on different trace types.
module Test.Mutagen.Tracer.Store
  ( -- * Trace backends
    TraceBackend (..)
  , STraceBackend (..)
  , withTraceBackend

    -- * Re-exports
  , TraceStoreImpl (..)
  )
where

import Test.Mutagen.Tracer.Store.API (TraceStoreImpl (..))
import Test.Mutagen.Tracer.Store.Bitmap ()
import Test.Mutagen.Tracer.Store.Tree ()
import Test.Mutagen.Tracer.Store.Types (TraceBackend (..))

{-------------------------------------------------------------------------------
-- * Trace backends
-------------------------------------------------------------------------------}

-- | Singleton version of 'TraceBackend'.
data STraceBackend trace where
  SBitmap :: STraceBackend Bitmap
  STree :: STraceBackend Tree

-- | Eliminate a 'TraceBackend' by providing a continuation that works for all
-- possible trace store implementations.
withTraceBackend
  :: TraceBackend
  -> (forall trace. (TraceStoreImpl trace) => STraceBackend trace -> r)
  -> r
withTraceBackend backend k =
  case backend of
    Bitmap -> k SBitmap
    Tree -> k STree
