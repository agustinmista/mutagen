{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RE.Types where

import Data.String
import Test.Mutagen
import qualified Test.Mutagen.TH as TH

-- | Regular expressions
data RE a
  = -- | Matches nothing
    Nil
  | -- | Matches the empty string
    Eps
  | -- | Matches a single token
    Atom a
  | -- | Kleene star, matches zero or more repetitions of an expression
    Star (RE a)
  | -- | Alternation, matches either of the two expressions
    Plus (RE a) (RE a)
  | -- | Concatenation, matches the first expression followed by the second
    Seq (RE a) (RE a)
  deriving (Eq, Ord, Show)

-- Automatically derive Arbitrary, Mutable, Lazy, and Fragmentable instances
TH.deriveAll ''RE

-- | ASCII characters
--
-- This exists just so that we can avoid Unicode characters in our tests.
newtype ASCII = ASCII Char
  deriving (Eq, Ord, Show)

instance {-# OVERLAPS #-} IsString [ASCII] where
  fromString = fmap ASCII

instance Arbitrary ASCII where
  arbitrary = ASCII <$> elements ['\x20' .. '\x7E']

instance Mutable ASCII where
  def = ASCII def
  inside pos mut | null pos = mut
  inside pos _ = error $ "inside: invalid position: " <> show pos
  mutate = const [Rand arbitrary]

instance Lazy ASCII where
  lazyNode pre (ASCII c) = ASCII (lazyNode pre c)

instance Fragmentable ASCII
