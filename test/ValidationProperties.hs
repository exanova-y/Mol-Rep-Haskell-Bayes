{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.QuickCheck
import Benzene (benzene)
import Chem.Molecule
import Chem.Dietz
import ValidatorDietz (validateMolecule)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Relabel a molecule according to a permutation of its AtomIds.
relabelMolecule :: Molecule -> [AtomId] -> Molecule
relabelMolecule m perm = Molecule atoms' bonds' systems'
  where
    oldIds   = M.keys (atoms m)
    mapping  = M.fromList (zip oldIds perm)
    rename i = mapping M.! i

    atoms' = M.fromList
      [ (rename i, a { atomID = rename i })
      | (i, a) <- M.toList (atoms m) ]

    bonds' = S.fromList
      [ mkEdge (rename i) (rename j)
      | Edge i j <- S.toList (localBonds m) ]

    systems' = M.map
      (\bs -> mkBondingSystem (sharedElectrons bs)
                               (S.fromList [ mkEdge (rename i) (rename j)
                                            | Edge i j <- S.toList (memberEdges bs) ])
                               (tag bs))
      (systems m)

-- Property: validation result is invariant under AtomId relabelling.
prop_permInvariant :: Property
prop_permInvariant = forAll genPerm $ \perm ->
  let mol' = relabelMolecule benzene perm
  in isRight (validateMolecule mol') === isRight (validateMolecule benzene)
  where
    genPerm = shuffle (M.keys (atoms benzene))
    isRight (Right _) = True
    isRight _         = False

-- Electron participation of atom v in bonding system bs.
ePart :: AtomId -> BondingSystem -> Double
ePart v bs =
  let degSv = fromIntegral $ length
                [ ()
                | Edge a b <- S.toList (memberEdges bs)
                , a == v || b == v ]
      s = fromIntegral (sharedElectrons bs)
      totalEdges = fromIntegral (S.size (memberEdges bs))
  in if totalEdges == 0 then 0 else s * degSv / (2 * totalEdges)

-- Property: each ring carbon gains 1 e- from the pi system plus sigma contributions.
prop_benzeneElectronAccounting :: Property
prop_benzeneElectronAccounting = conjoin
  [ counterexample ("Atom " ++ show i) $
      let sigma = fromIntegral (length (neighborsSigma benzene i))
          system = sum [ ePart i bs | bs <- M.elems (systems benzene) ]
      in system === 1.0 .&&. sigma + system === 4.0
  | i <- ringCarbons ]
  where
    ringCarbons = map AtomId [1..6]

main :: IO ()
main = do
  quickCheck prop_permInvariant
  quickCheck prop_benzeneElectronAccounting
