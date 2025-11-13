{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Entry point for Template Haskell based derivations of Mutagen type classes.
--
-- Currently, this supports deriving instances for the 'Arbitrary', 'Mutable',
-- 'Lazy', and 'Fragmentable' type classes.
--
-- @
--    {-# LANGUAGE TemplateHaskell #-}
--    import qualified Test.Mutagen.TH as TH
--
--    data MyType = Foo | Bar | Baz Int | Qux String
--      deriving (Show, Eq, Ord)
--
--    -- Derive all supported instances
--    TH.deriveAll ''MyType
--
--    -- Or, alternative, derive individual instances
--    TH.deriveInstance ''MyType ''Arbitrary
--    TH.deriveInstance ''MyType ''Fragmentable
--
--    -- While optionally setting custom derivation options
--    TH.deriveInstanceWithOptions
--      TH.defaultTHOptions
--        { TH.optIgnore = ['Qux] -- ignore the @Qux@ constructor
--        , TH.optDef = Just 'Bar -- derive @def = Bar@
--        }
--      ''MyType
--      ''Mutable
-- @
module Test.Mutagen.TH
  ( -- * Derivation options
    THOptions (..)
  , defaultTHOptions

    -- * Derivation dispatchers
  , deriveAll
  , deriveInstance
  , deriveInstanceWithOptions
  )
where

import Control.Monad.Extra (concatMapM)
import Language.Haskell.TH (Dec, Name, Q)
import Language.Haskell.TH.Desugar (sweeten)
import Test.Mutagen (Arbitrary, Fragmentable, Lazy, Mutable)
import Test.Mutagen.TH.Arbitrary (deriveArbitrary)
import Test.Mutagen.TH.Fragmentable (deriveFragmentable)
import Test.Mutagen.TH.Lazy (deriveLazy)
import Test.Mutagen.TH.Mutable (deriveMutable)
import Test.Mutagen.TH.Util (dump, mutagenError, mutagenLog)

{-------------------------------------------------------------------------------
-- * Derivation options
-------------------------------------------------------------------------------}

-- | Options for deriving instances using Template Haskell.
data THOptions
  = THOptions
  { optIgnore :: [Name]
  -- ^ Ignore certain constructors during the derivation process.
  , optDefault :: Maybe Name
  -- ^ Default value for 'Test.Mutagen.Mutation.def' when deriving 'Mutable'.
  }

-- | Default derivation options.
defaultTHOptions :: THOptions
defaultTHOptions =
  THOptions
    { optIgnore = []
    , optDefault = Nothing
    }

{-------------------------------------------------------------------------------
-- * Derivation dispatchers
-------------------------------------------------------------------------------}

-- | Derive all supported type class instances for a given data type.
deriveAll :: Name -> Q [Dec]
deriveAll typeName =
  concatMapM
    (deriveInstance typeName)
    [ ''Arbitrary
    , ''Mutable
    , ''Lazy
    , ''Fragmentable
    ]

-- | Derive a single instance for a given data type and type class.
deriveInstance :: Name -> Name -> Q [Dec]
deriveInstance = deriveInstanceWithOptions defaultTHOptions

-- | Derive a single custom instance for a given data type and type class.
deriveInstanceWithOptions :: THOptions -> Name -> Name -> Q [Dec]
deriveInstanceWithOptions opts typeName className = do
  mutagenLog
    $ "deriving instance "
      <> dump className
      <> " for "
      <> dump typeName
      <> ignoringString
  ins <- sweeten <$> derive
  mutagenLog $ "derived instance: " <> dump ins
  return ins
  where
    ignoringString
      | null (optIgnore opts) =
          ""
      | otherwise =
          " (ignoring: " <> dump (optIgnore opts) <> ")"
    derive
      | className == ''Arbitrary =
          deriveArbitrary typeName (optIgnore opts)
      | className == ''Mutable =
          deriveMutable typeName (optIgnore opts) (optDefault opts)
      | className == ''Lazy =
          deriveLazy typeName (optIgnore opts)
      | className == ''Fragmentable =
          deriveFragmentable typeName (optIgnore opts)
      | otherwise =
          mutagenError "type class not supported" [className]
