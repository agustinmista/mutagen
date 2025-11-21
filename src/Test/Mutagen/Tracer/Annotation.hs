{-# LANGUAGE DeriveDataTypeable #-}

-- | Annotations used to instruct the Mutagen tracer plugin what to instrument
module Test.Mutagen.Tracer.Annotation
  ( -- * Annotations
    TRACE (..)
  )
where

import Data.Data (Data)

{-------------------------------------------------------------------------------
-- * Tracing annotations
-------------------------------------------------------------------------------}

-- | Tell the tracer plugin to trace this function.
--
-- For example:
-- @
--  {-# ANN myFunction TRACE #-}
--  myFunction :: Int -> Int
--  myFunction x = x + 1
-- @
data TRACE = TRACE
  deriving (Data)
