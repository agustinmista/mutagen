{-# LANGUAGE TemplateHaskell #-}

-- | Derive an 'Arbitrary' instance for a given data type.
module Test.Mutagen.TH.Arbitrary
  ( deriveArbitrary
  )
where

import Control.Monad.Extra (ifM)
import Data.Bool (bool)
import Data.List (partition)
import Language.Haskell.TH
  ( Lit (..)
  , Name
  , Q
  , Type (..)
  , isInstance
  , newName
  , varE
  )
import Language.Haskell.TH.Desugar
  ( DClause (..)
  , DCon (..)
  , DDec (..)
  , DExp (..)
  , DLetDec (..)
  , DMatch (..)
  , DPat (..)
  , DType (..)
  , dCaseE
  , dsExp
  )
import Test.Mutagen
  ( Arbitrary
  , Arbitrary1
  , Arbitrary2
  , Gen
  , arbitrary
  , liftArbitrary
  , liftArbitrary2
  , oneof
  , sized
  )
import Test.Mutagen.TH.Util
  ( applyTyVars
  , dConFields
  , dConFieldsTypes
  , dConName
  , dTyVarBndrName
  , dump
  , isMaybeOf
  , mkApplicativeDExp
  , mkListDExp
  , mutagenLog
  , reifyTypeDef
  , (.==.)
  )

{-------------------------------------------------------------------------------
-- * Deriving Arbitrary instances
-------------------------------------------------------------------------------}

-- | Derive an 'Arbitrary' instance for the given data type.
deriveArbitrary :: Name -> [Name] -> Q [DDec]
deriveArbitrary typeName ignoredCons = do
  mutagenLog
    $ "deriving Lazy instance for "
      <> dump typeName
      <> bool (" (ignoring: " <> dump ignoredCons <> ")") "" (null ignoredCons)
  -- Reify the type definition
  (dtvbs, dcons) <- reifyTypeDef typeName
  -- Apply the context type variables to the type name to get 'Type'-kinded
  -- target type to derive the instance for
  let targetType = applyTyVars typeName dtvbs
  -- Keep only the constructors that are not ignored
  let wantedCons = filter (\con -> dConName con `notElem` ignoredCons) dcons
  -- Split terminal and recursive constructors
  let (recs, terms) =
        partition
          (\con -> targetType `elem` dConFieldsTypes (dConFields con))
          wantedCons
  -- Create some fresh TH variables
  gen_ <- newName "gen"
  size_ <- newName "size"
  -- Create generator expressions for each constructor
  recGens <- mapM (mkConGen targetType gen_ size_) recs
  termGens <- mapM (mkConGen targetType gen_ size_) terms
  -- Build the Arbitrary instance
  let insCxt = [DConT ''Arbitrary `DAppT` DVarT (dTyVarBndrName tvb) | tvb <- dtvbs]
  let insTy = DConT ''Arbitrary `DAppT` targetType
  let baseCase =
        DMatch
          (DLitP (IntegerL 0))
          (DVarE 'oneof `DAppE` mkListDExp termGens)
  let recCase =
        DMatch
          DWildP
          (DVarE 'oneof `DAppE` mkListDExp (termGens <> recGens))
  let genDecl =
        DFunD
          gen_
          [ DClause
              [DVarP size_]
              ( dCaseE
                  (DVarE size_)
                  [baseCase, recCase]
              )
          ]
  let insGen = DLetE [genDecl] (DVarE 'sized `DAppE` DVarE gen_)
  let insBody = [DLetDec (DFunD 'arbitrary [DClause [] insGen])]
  return [DInstanceD Nothing Nothing insCxt insTy insBody]

-- | Create the appropriate generator for a constructor.
mkConGen :: DType -> Name -> Name -> DCon -> Q DExp
mkConGen targetType gen_ size_ (DCon _ _ cname cfields _) = do
  fieldGens <-
    mapM
      (mkConFieldGen targetType gen_ size_)
      (dConFieldsTypes cfields)
  return (mkApplicativeDExp cname fieldGens)

-- | Create the appropriate generator for a constructor field based
mkConFieldGen :: DType -> Name -> Name -> DType -> Q DExp
mkConFieldGen targetType gen_ size_ fieldTy =
  dsExp =<< mkGen fieldTy
  where
    mkGen ty
      -- The field is (immediately) recursive
      -- ==> Use the reference to the recursive generator
      | ty .==. targetType =
          [e|$(varE gen_) (max 0 ($(varE size_) - 1))|]
      -- Some special cases in between
      -- ==> use special 'Gen' combinators accordingly
      | Just a <- isMaybeOf ty =
          [e|sizedMaybe $(mkGen a)|]
      -- Types of kind `Type -> Type`
      -- ==> Check for 'Arbitrary1' instance, otherwise fallback to 'arbitrary'
      | (DConT f `DAppT` a) <- ty =
          ifM
            (isInstance ''Arbitrary1 [ConT f])
            [e|liftArbitrary $(mkGen a)|]
            [e|arbitrary|]
      -- Types of kind `Type -> Type -> Type`
      -- ==> Check for 'Arbitrary2' instance, otherwise fallback to 'arbitrary'
      | (DConT f `DAppT` t1 `DAppT` t2) <- ty =
          ifM
            (isInstance ''Arbitrary2 [ConT f])
            [e|liftArbitrary2 $(mkGen t1) $(mkGen t2)|]
            [e|arbitrary|]
      -- The field type is something else
      -- ==> Assume it has an 'Arbitrary' instance and hope for the best
      | otherwise =
          [e|arbitrary|]

-- | Create a sized 'Maybe' generator that returns 'Nothing' when size is 0.
--
-- NOTE: we found this useful empirically for types where it's really hard to
-- randomly generate valid random values for. Defaulting to 'Nothing' at size 0
-- helps avoid generating invalid optionals.
sizedMaybe :: Gen a -> Gen (Maybe a)
sizedMaybe gen = sized $ \size ->
  if size == 0
    then return Nothing
    else liftArbitrary gen
