{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Trace store implementations based on different trace types
module Test.Mutagen.Tracer.Store
  ( -- * Trace types
    TraceType (..)
  , STraceType (..)
  , withTraceType

    -- * Re-exports
  , TraceStoreImpl (..)
  )
where

import Test.Mutagen.Tracer.Store.API (TraceStoreImpl (..))
import Test.Mutagen.Tracer.Store.Bitmap ()
import Test.Mutagen.Tracer.Store.Tree ()
import Test.Mutagen.Tracer.Store.Types (TraceType (..))

{-------------------------------------------------------------------------------
-- * Trace type selection at runtime
-------------------------------------------------------------------------------}

-- | Singleton version of 'TraceType'
data STraceType trace where
  SBitmap :: STraceType Bitmap
  STree :: STraceType Tree

-- | Eliminate a 'TraceType' by providing a continuation that works for all
-- possible trace types
withTraceType
  :: TraceType
  -> (forall trace. (TraceStoreImpl trace) => STraceType trace -> r)
  -> r
withTraceType method k =
  case method of
    Bitmap -> k SBitmap
    Tree -> k STree
