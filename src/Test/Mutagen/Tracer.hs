-- | Public API for the Mutagen Tracer plugin
module Test.Mutagen.Tracer
  ( plugin
  , __trace__
  , TRACE (..)
  , TraceNode
  )
where

import Test.Mutagen.Tracer.Annotation (TRACE (..))
import Test.Mutagen.Tracer.Plugin (plugin)
import Test.Mutagen.Tracer.Trace (TraceNode, __trace__)
