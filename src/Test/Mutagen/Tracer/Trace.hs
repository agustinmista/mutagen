-- | Tracing executed AST nodes via some
module Test.Mutagen.Tracer.Trace
  ( -- * Tracing
    TraceNode
  , __trace__
  , Trace (..)
  , addTraceNode
  , resetTraceRef
  , readTraceRef
  , withTrace
  , truncateTrace
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
-- * Tracing
-------------------------------------------------------------------------------}

-- | AST node identifiers
type TraceNode = Int

-- | Trace the evaluation of a given 'TraceNode'.
--
-- This function is intended to be used by the GHC plugin to
__trace__ :: TraceNode -> a -> a
__trace__ n expr = unsafePerformIO (addTraceNode n >> return expr)
{-# INLINE __trace__ #-}

-- | A dynamic trace keeping track of executed trace nodes
newtype Trace = Trace {unTrace :: [TraceNode]}
  deriving (Show)

-- | Global 'IORef' holding the current execution trace
globalTraceRef :: IORef Trace
globalTraceRef = unsafePerformIO (newIORef (Trace []))
{-# NOINLINE globalTraceRef #-}

-- | Add a new entry to the current global trace
addTraceNode :: TraceNode -> IO ()
addTraceNode n =
  atomicModifyIORef' globalTraceRef $ \(Trace entries) ->
    (Trace (n : entries), ())

-- | Reset the global trace
resetTraceRef :: IO ()
resetTraceRef =
  atomicModifyIORef' globalTraceRef $ \_ ->
    (Trace [], ())

-- | Read the current global trace
readTraceRef :: IO Trace
readTraceRef = do
  Trace entries <- readIORef globalTraceRef
  return (Trace (reverse entries))

-- | Run a computation and obtain its trace
withTrace :: IO a -> IO (a, Trace)
withTrace io = do
  resetTraceRef
  a <- io
  tr <- readTraceRef
  return (a, tr)

-- Truncate a trace to a given length
truncateTrace :: Int -> Trace -> Trace
truncateTrace n (Trace entries) =
  Trace (take n entries)
