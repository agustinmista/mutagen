-- | Exception handling utilities
module Test.Mutagen.Exception
  ( -- * Exception handling utilities
    AnException
  , tryEvaluate
  , tryEvaluateIO
  , evaluate
  , finally
  , discard
  , isDiscard
  )
where

import qualified Control.Exception as Exception

{-------------------------------------------------------------------------------
-- * Exception handling utilities
-------------------------------------------------------------------------------}

-- | A general exception type
type AnException = Exception.SomeException

-- | Evaluate a value to weak head normal form, catching any exceptions
tryEvaluate :: a -> IO (Either AnException a)
tryEvaluate x = tryEvaluateIO (return x)

-- | Evaluate an IO action to weak head normal form, catching any exceptions
tryEvaluateIO :: IO a -> IO (Either AnException a)
tryEvaluateIO m =
  Exception.tryJust notAsync (m >>= Exception.evaluate)
  where
    notAsync :: AnException -> Maybe AnException
    notAsync e = case Exception.fromException e of
      Just (Exception.SomeAsyncException _) -> Nothing
      Nothing -> Just e

-- | Evaluate a value to weak head normal form
evaluate :: a -> IO a
evaluate = Exception.evaluate

-- | Ensure that a cleanup action is run after an IO action, even if
-- an exception is thrown
finally :: IO a -> IO b -> IO a
finally = Exception.finally

-- | A special error value. If a property evaluates 'discard', it causes
-- Mutagen to discard the current test case. This can be useful if you want
-- to discard the current test case, but are somewhere you can't use
-- 'Test.Mutagen.==>', such as inside a generator.
discard :: a
isDiscard :: AnException -> Bool
(discard, isDiscard) =
  (error msg, isDiscard')
  where
    msg = "DISCARD. You should not see this exception, it is internal to Mutagen."
    isDiscard' e =
      case Exception.fromException e of
        Just (Exception.ErrorCall msg') -> msg' == msg
        _ -> False
