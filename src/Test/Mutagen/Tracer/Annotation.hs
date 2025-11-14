{-# LANGUAGE DeriveDataTypeable #-}

-- | Annotations used to instruct the Mutagen tracer plugin what to instrument.
module Test.Mutagen.Tracer.Annotation
  ( -- * Annotations
    TRACE (..)
  )
where

import Data.Data (Data)

{-------------------------------------------------------------------------------
-- * Tracing annotations
-------------------------------------------------------------------------------}

data TRACE = TRACE
  deriving (Data)
