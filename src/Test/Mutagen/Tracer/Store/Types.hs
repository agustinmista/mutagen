{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Runtime trace types.
module Test.Mutagen.Tracer.Store.Types
  ( -- * Tracing backends
    TraceBackend (..)
  )
where

{-------------------------------------------------------------------------------
-- * Tracing backends
-------------------------------------------------------------------------------}

-- | Supported tracing backends.
data TraceBackend
  = -- | Edge based tracing using a bitmap.
    Bitmap
  | -- | Path based tracing using a rose tree.
    Tree
  deriving (Eq, Show)
