-- | Public API for the Mutagen Tracer plugin
module Test.Mutagen.Tracer
  ( plugin
  , __trace__
  , TRACE (..)
  )
where

import Test.Mutagen.Tracer.Annotation (TRACE (..))
import Test.Mutagen.Tracer.Plugin (plugin)
import Test.Mutagen.Tracer.Trace (__trace__)
