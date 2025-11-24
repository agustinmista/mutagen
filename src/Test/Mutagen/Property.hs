{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Combinators for constructing and running properties
module Test.Mutagen.Property
  ( -- * Property arguments
    IsArgs
  , Args (..)
  , Result (..)
  , pattern Passed
  , pattern Failed
  , pattern Discarded
  , Prop
  , unProp
  , mapProp
  , protectProp
  , (==>)
  , discardAfter
  , IsProp (..)
  , Property (..)
  , mapProperty
  , forAll
  , expectFailure
  , Testable (..)
  )
where

import Data.Typeable (Typeable, cast)
import System.Timeout (timeout)
import Test.Mutagen.Exception (AnException, discard, isDiscard, tryEvaluateIO)
import Test.Mutagen.Fragment (Fragmentable (..))
import Test.Mutagen.Lazy (Lazy (..))
import Test.Mutagen.Mutation (Mutable (..))
import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
-- * Property arguments
-------------------------------------------------------------------------------}

-- | Constraints needed for types that can be used as property arguments
type IsArgs a =
  ( Show a
  , Eq a
  , Ord a
  , Typeable a
  , Arbitrary a
  , Fragmentable a
  , Mutable a
  , Lazy a
  )

-- | Test arguments hidden behind an existential
data Args = forall a. (IsArgs a) => Args a

instance Show Args where
  show (Args arg) = show arg

instance Mutable Args where
  mutate (Args a) = fmap Args <$> mutate a
  inside pos mut (Args a) = fmap Args <$> inside pos mut a
  positions (Args a) = positions a

instance Lazy Args where
  lazy (Args a) = Args (lazy a)
  lazyNode pre (Args a) = Args (lazyNode pre a)

instance Eq Args where
  Args a == Args b =
    case cast b of
      Nothing -> False
      Just b' -> a == b'

instance Ord Args where
  compare (Args a) (Args b) =
    case cast b of
      Just b' -> compare a b'
      Nothing -> LT

instance Fragmentable Args where
  fragmentize (Args a) = fragmentize a

{-------------------------------------------------------------------------------
-- * Property results
-------------------------------------------------------------------------------}

-- | Result of executing a property
data Result = Result
  { resultOk :: Maybe Bool
  -- ^ 'Just True' for passed, 'Just False' for failed, 'Nothing' for discarded
  , resultException :: Maybe AnException
  -- ^ Exception raised during evaluation, if any
  , resultReason :: Maybe String
  -- ^ Reason for failure or discarding, if any
  , resultExpect :: Bool
  -- ^ Whether the test was expected to pass or fail
  }
  deriving (Show)

{-# COMPLETE Passed, Failed, Discarded :: Result #-}

-- | Pattern synonyms for a successful t'Result'
pattern Passed :: Result
pattern Passed <- Result{resultOk = Just True}

-- | Pattern synonyms for a failed t'Result'
pattern Failed :: Result
pattern Failed <- Result{resultOk = Just False}

-- | Pattern synonyms for a discarded t'Result'
pattern Discarded :: Result
pattern Discarded <- Result{resultOk = Nothing}

-- ** Result constructors

bool :: Bool -> Result
bool b = Result (Just b) Nothing Nothing True

failed :: Result
failed = bool False

discarded :: Result
discarded = Result Nothing Nothing Nothing True

exception :: AnException -> Result
exception e
  | isDiscard e =
      discarded
        { resultException = Just e
        , resultReason = Just "evaluated 'discard'"
        }
  | otherwise =
      failed
        { resultException = Just e
        , resultReason = Just "exception"
        }

{-------------------------------------------------------------------------------
-- * Executable properties
-------------------------------------------------------------------------------}

-- | Executable properties as IO computations producing results
newtype Prop = Prop
  { unProp :: IO Result
  -- ^ Extract the IO computation from a Prop
  }

-- | Map a function over the result of a prop
mapProp :: (Result -> Result) -> Prop -> Prop
mapProp f = Prop . fmap f . unProp

-- | Protect a prop against exceptions during evaluation
protectProp :: Prop -> Prop
protectProp (Prop io) = Prop $ do
  let force t = resultOk t == Just False `seq` t
  res <- tryEvaluateIO (fmap force io)
  case res of
    Left e -> return (exception e)
    Right r -> return r

-- | Implication combinator for properties
(==>) :: (IsProp a) => Bool -> a -> Prop
(==>) True post = prop post
(==>) False _ = prop discarded

infixr 2 ==>

-- | Discard a property if it takes more than some milliseconds
discardAfter :: (IsProp a) => Int -> a -> Prop
discardAfter millis a = Prop $ do
  let iot = unProp (prop a)
  mbt <- timeout (millis * 1000) iot
  maybe discard return mbt

-- | Types that can produce props
class IsProp a where
  prop :: a -> Prop

instance IsProp Prop where
  prop = id

instance IsProp Result where
  prop t = Prop (return t)

instance IsProp Bool where
  prop b = Prop (return (bool b))

instance (IsProp a) => IsProp (IO a) where
  prop ior = Prop $ do
    r <- ior
    unProp (prop r)

{-------------------------------------------------------------------------------
-- * Testable properties
-------------------------------------------------------------------------------}

-- | Properties encapsulating generators of arguments and runner functions
data Property = Property (Gen Args) (Args -> Prop)

-- | Map a function over the inner executable t'Prop' of a property
mapProperty :: (Prop -> Prop) -> Property -> Property
mapProperty f (Property g p) = Property g (f . p)

-- | Universal quantification over generated arguments
forAll :: (IsArgs a, IsProp b) => Gen a -> (a -> b) -> Property
forAll gen f =
  Property (Args <$> gen) $ \(Args as) ->
    prop (f (unsafeCoerce as))

-- | Expect a property to fail
expectFailure :: (Testable prop) => prop -> Property
expectFailure p =
  mapProperty
    (mapProp (\test -> test{resultExpect = False}))
    (property p)

-- ** Testable type class

-- | A class for testable properties
class Testable a where
  property :: a -> Property

-- | Properties are trivially testable
instance Testable Property where
  property p = p

instance Testable Bool where
  property b = property (\() -> b)

-- | Testable properties with one argument
instance (IsArgs a, IsProp b) => Testable (a -> b) where
  property = forAll arbitrary
