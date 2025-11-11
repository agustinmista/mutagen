{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Mutagen.TH.Mutable
  ( deriveMutable
  )
where

import Control.Monad (forM, guard)
import Data.Bool (bool)
import Data.List (sortOn)
import Language.Haskell.TH
  ( Lit (..)
  , Name
  , Q
  , newName
  )
import Language.Haskell.TH.Desugar
  ( DClause (..)
  , DCon (..)
  , DDec (..)
  , DExp (..)
  , DLetDec (..)
  , DPat (..)
  , DType (..)
  , dLamE
  , dPatToDExp
  , mkTupleDExp
  )
import Test.Mutagen.Fragment
  ( sampleFragments
  )
import Test.Mutagen.Mutant
  ( Mutant (..)
  )
import Test.Mutagen.Mutation
  ( Mutable
  , def
  , inside
  , invalidPosition
  , mutate
  , node
  , positions
  , wrap
  )
import Test.Mutagen.TH.Util
  ( applyTyVars
  , createDPat
  , dConFields
  , dConFieldsNum
  , dConFieldsTypes
  , dConName
  , dTyVarBndrName
  , dump
  , mkConDExp
  , mkListDExp
  , mutagenError
  , mutagenLog
  , reifyTypeDef
  )

{-------------------------------------------------------------------------------
-- * Deriving Mutable instances
-------------------------------------------------------------------------------}

-- | Derive a 'Mutable' instance for the given data type.
deriveMutable :: Name -> [Name] -> Maybe Name -> Q [DDec]
deriveMutable typeName ignoredCons mbDef = do
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
  -- Derive each function in the Mutable class separately
  insDef <- deriveDef targetType mbDef wantedCons
  insPositions <- derivePositions wantedCons
  insInside <- deriveInside wantedCons
  insMutate <- deriveMutate wantedCons
  -- Build the Mutable instance
  let insCxt = [DConT ''Mutable `DAppT` DVarT (dTyVarBndrName tvb) | tvb <- dtvbs]
  let insTy = DConT ''Mutable `DAppT` targetType
  let insBody = concat [insDef, insPositions, insInside, insMutate]
  return [DInstanceD Nothing Nothing insCxt insTy insBody]

-- | Derive the 'def' method for the 'Mutable' type class.
deriveDef :: DType -> Maybe Name -> [DCon] -> Q [DDec]
deriveDef dty mbDef cons = do
  defValue <-
    case mbDef of
      Just var -> do
        return $ DVarE var
      Nothing -> do
        let terms = filter (notElem dty . dConFieldsTypes . dConFields) cons
        let sorted = sortOn (dConFieldsNum . dConFields) terms
        smallest <-
          case sorted of
            [] ->
              mutagenError
                ( "could not find a proper constructor to derive 'def' with, "
                    <> "please define a default value manually via 'optDefault'"
                )
                [sorted]
            con : _ ->
              return con
        return
          $ mkConDExp
            (dConName smallest)
            (replicate (dConFieldsNum (dConFields smallest)) (DVarE 'def))
  return [DLetDec (DFunD 'def [DClause [] defValue])]

-- | Derive the 'positions' method for the 'Mutable' type class.
derivePositions :: [DCon] -> Q [DDec]
derivePositions cons = do
  clauses <-
    forM cons $ \con -> do
      (vars, pat) <- createDPat con
      let clauseBody =
            DVarE 'node
              `DAppE` mkListDExp
                [ mkTupleDExp
                    [ DLitE (IntegerL n)
                    , DVarE 'positions `DAppE` DVarE var
                    ]
                | (n, var) <- zip [0 ..] vars
                ]
      return (DClause [pat] clauseBody)
  return [DLetDec (DFunD 'positions clauses)]

-- | Derive the 'inside' method for the 'Mutable' type class.
deriveInside :: [DCon] -> Q [DDec]
deriveInside cons = do
  pos_ <- newName "pos"
  mut_ <- newName "mut"
  x_ <- newName "x"
  -- First clause
  let firstClause =
        DClause
          [DConP '[] [] [], DVarP mut_, DVarP x_]
          (DVarE mut_ `DAppE` DVarE x_)
  -- Recursive constructor clauses
  conClauses <-
    forM cons $ \con -> do
      (vars, pat) <- createDPat con
      forM [0 .. length vars - 1] $ \idx -> do
        let posPat =
              DConP '(:) [] [DLitP (IntegerL (fromIntegral idx)), DVarP pos_]
        let mutPat =
              DVarP mut_
        let insideExpr =
              DVarE 'inside
                `DAppE` DVarE pos_
                `DAppE` DVarE mut_
                `DAppE` DVarE (vars !! idx)
        let lamExpr =
              dLamE
                [DVarP x_]
                ( mkConDExp
                    (dConName con)
                    [DVarE v | v <- replaceAt idx x_ vars]
                )
        let clauseBody =
              DVarE 'wrap
                `DAppE` insideExpr
                `DAppE` lamExpr
        return
          $ DClause
            [posPat, mutPat, pat]
            clauseBody
  -- Last clause (error message)
  let lastClause =
        DClause
          [DVarP pos_, DWildP, DWildP]
          (DVarE 'invalidPosition `DAppE` DVarE pos_)
  -- Combine all clauses
  let clauses = [firstClause] <> concat conClauses <> [lastClause]
  return [DLetDec (DFunD 'inside clauses)]

-- | Derive the 'mutate' method for the 'Mutable' type class.
deriveMutate :: [DCon] -> Q [DDec]
deriveMutate cons = do
  clauses <-
    forM cons $ \con -> do
      (vars, pat) <- createDPat con
      -- Fragment mutation
      let fragMutants =
            DConE 'Frag `DAppE` (DVarE 'sampleFragments `DAppE` dPatToDExp pat)
      -- Pure mutations
      let pureMutants =
            [ DConE 'Pure `DAppE` mutatedCon
            | let fieldTypes = zip vars (dConFieldsTypes (dConFields con))
            , let name = dConName con
            , mutatedCon <- mutateCon name fieldTypes cons
            ]
      let clauseBody = mkListDExp (fragMutants : pureMutants)
      return (DClause [pat] clauseBody)
  return [DLetDec (DFunD 'mutate clauses)]

-- | Generate mutated constructor expressions
--
-- NOTE: this looks kinda funky because it's is defined using the list monad :)
mutateCon :: Name -> [(Name, DType)] -> [DCon] -> [DExp]
mutateCon name fieldTypes cons = do
  con <- cons
  mutation <- validMutations con
  guard (mutation /= nullMutation)
  return mutation
  where
    -- Combine valid field substitutions into saturated constructor expressions
    validMutations con =
      combineMutatedFields
        (DConE (dConName con))
        (validFieldSubstitutions con)
    -- Recursively build all valid combinations of mutated fields
    combineMutatedFields acc [] = do
      return acc
    combineMutatedFields acc (fields : rest) = do
      field <- fields
      combineMutatedFields (acc `DAppE` DVarE field) rest
    -- For each field of a constructor, find valid substitutions of the same
    -- type, using this instance's 'def' value if none are found
    validFieldSubstitutions con =
      [ if null subst then ['def] else subst
      | fty <- dConFieldsTypes (dConFields con)
      , let subst = validSubstitutions fty
      ]
    -- Find all variables that can substitute a given field type
    validSubstitutions ty' = do
      (field, ty) <- fieldTypes
      guard (ty' == ty)
      return field
    -- The null mutation is the one that leaves all fields unchanged
    nullMutation =
      mkConDExp name [DVarE field | (field, _) <- fieldTypes]

-- | Replace the element at the given index in a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n y xs = [if i == n then y else x | (x, i) <- zip xs [0 ..]]
