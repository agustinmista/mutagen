{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Runntime types.
module Test.Mutagen.Tracer.Store.Types
  ( -- * Trace types
    TraceType (..)
  )
where

{-------------------------------------------------------------------------------
-- * Trace types
-------------------------------------------------------------------------------}

-- | Runtime trace types
data TraceType = Bitmap | Tree
  deriving (Eq, Show)
