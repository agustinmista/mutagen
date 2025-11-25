-- | Public API for the Mutagen Tracer plugin.
module Test.Mutagen.Tracer
  ( trace
  , TRACE (..)
  , TraceNode
  , withTrace
  , truncateTrace
  )
where

import Test.Mutagen.Tracer.Annotation (TRACE (..))
import Test.Mutagen.Tracer.Trace (TraceNode, trace, truncateTrace, withTrace)
