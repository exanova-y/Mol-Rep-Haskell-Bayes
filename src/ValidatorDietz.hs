{-# LANGUAGE OverloadedStrings #-}
module ValidatorDietz
  ( ValidationError(..)
  , validateMolecule
  ) where

import Chem.Molecule
import Chem.Dietz
import Constants (getMaxBondsSymbol)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Specific validation errors.
data ValidationError
  = SelfBond Edge                    -- ^ bond from an atom to itself
  | MissingAtom Edge AtomId          -- ^ bond references a missing atom
  | SystemMissingAtom SystemId Edge  -- ^ system references a missing atom
  | ElectronLimitExceeded AtomId Double Double -- ^ actual vs allowed electrons
  deriving (Eq, Show)

-- | Validate a molecule according to Dietz bonding rules.
validateMolecule :: Molecule -> Either [ValidationError] Molecule
validateMolecule m =
  case errs of
    [] -> Right m
    _  -> Left errs
  where
    atomSet = M.keysSet (atoms m)

    -- Check each sigma bond.
    sigmaErrs = concat
      [ checkEdge e
      | e@(Edge i j) <- S.toList (localBonds m) ]
    checkEdge e@(Edge i j)
      | i == j = [SelfBond e]
      | not (i `S.member` atomSet) = [MissingAtom e i]
      | not (j `S.member` atomSet) = [MissingAtom e j]
      | otherwise = []

    -- Check bonding systems for nonexistent atoms.
    systemErrs =
      [ SystemMissingAtom sid e
      | (sid, bs) <- M.toList (systems m)
      , e@(Edge i j) <- S.toList (memberEdges bs)
      , not (i `S.member` atomSet) || not (j `S.member` atomSet)
      ]

    -- Electron accounting per atom.
    electronErrs =
      [ ElectronLimitExceeded i count limit
      | (i, atom) <- M.toList (atoms m)
      , let sigma = fromIntegral (length (neighborsSigma m i))
            system = sum [ ePart i bs | bs <- M.elems (systems m) ]
            count = sigma + system
            limit = fromIntegral (2 * getMaxBondsSymbol (symbol (attributes atom)))
      , count > limit
      ]

    -- Electron participation of atom v in bonding system bs.
    ePart v bs =
      let degSv = fromIntegral $ length
                    [ ()
                    | Edge a b <- S.toList (memberEdges bs)
                    , a == v || b == v ]
          s = fromIntegral (sharedElectrons bs)
          totalEdges = fromIntegral (S.size (memberEdges bs))
      in if totalEdges == 0 then 0 else s * degSv / (2 * totalEdges)

    errs = sigmaErrs ++ systemErrs ++ electronErrs
