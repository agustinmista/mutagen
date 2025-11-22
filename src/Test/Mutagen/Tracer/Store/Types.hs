{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Runtime trace types
module Test.Mutagen.Tracer.Store.Types
  ( -- * Tracing backends
    TraceBackend (..)
  )
where

{-------------------------------------------------------------------------------
-- * Tracing backends
-------------------------------------------------------------------------------}

-- | Tracing backends
data TraceBackend = Bitmap | Tree
  deriving (Eq, Show)
