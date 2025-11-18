{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Runtime trace types
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
