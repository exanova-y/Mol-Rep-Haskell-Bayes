{-# LANGUAGE RecordWildCards #-}
module TestInference where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (tails)
import Chem.Molecule
import Chem.Dietz
import LazyPPL
import Parser
import Distr
import Control.Monad
import ExtraF
import Constants
import ValidatorDietz (validateMolecule)
import Text.Printf (printf)
import Data.Monoid (Product(..))
import Numeric.Log (Log)


--------------------------------------------------------------------------------
-- | Read the observed molecule from file.
observedMoleculeIO :: IO Molecule
observedMoleculeIO = do
  let db1FilePath = "./molecules/water.sdf"
  moleculesWithLogP <- parseDB1File db1FilePath
  case moleculesWithLogP of
    [] -> error "No molecule found in file!"
    ((molecule, _):_) -> return molecule

--------------------------------------------------------------------------------
-- | A generative model for a molecule that takes an observed molecule for scoring.
moleculeModel :: Molecule -> Meas Molecule
moleculeModel observed = do
  let numAtoms = 3
  atomsUnnumbered <- replicateM numAtoms $ do
    symbol <- sample $ uniformD [C, N, O, H]
    coord  <- sampleCoordinate
    let attr      = elementAttributes symbol
        shellsVar = elementShells symbol
    return $ Atom { atomID = AtomId 0
                  , attributes = attr
                  , coordinate = coord
                  , shells = shellsVar
                  , formalCharge = 0 }
  let atomsList = zipWith (\i atom -> atom { atomID = AtomId i }) [1..] atomsUnnumbered
      ids   = map atomID atomsList
      atoms = M.fromList [ (atomID a, a) | a <- atomsList ]
      initialMol = Molecule { atoms = atoms, localBonds = S.empty, systems = M.empty }
      possiblePairs = [(i, j) | (i:rest) <- tails ids, j <- rest]

  (molecule, _) <- foldM sampleBond (initialMol, 1) possiblePairs

  -- Score the model based on the distance to the observed molecule.
  let distance = hausdorffDistance molecule observed
  score $ normalPdf 0 1 (unAngstrom distance)

  return molecule

-- | Randomly generate a bond graph for a molecule with n atoms.
--   For each unique pair (i,j) (with i < j), we randomly decide whether
--   to include a bond. If a bond is included, we sample its bond order from [1,2,3].


--------------------------------------------------------------------------------
-- Sample a coordinate from a normal distribution.
sampleCoordinate :: Meas Coordinate
sampleCoordinate = do
  x <- sample $ normal 0 1
  y <- sample $ normal 0 1
  z <- sample $ normal 0 1
  return $ Coordinate (mkAngstrom x) (mkAngstrom y) (mkAngstrom z)

--------------------------------------------------------------------------------
-- Compute the Hausdorff distance between two molecules, returned in Angstroms.
hausdorffDistance :: Molecule -> Molecule -> Angstrom
hausdorffDistance mol1 mol2 =
  let atoms1 = M.elems (atoms mol1)
      atoms2 = M.elems (atoms mol2)
      d1 = [minimum [distanceAngstrom a1 a2 | a2 <- atoms2] | a1 <- atoms1]
      d2 = [minimum [distanceAngstrom a2 a1 | a1 <- atoms1] | a2 <- atoms2]
  in maximum (d1 ++ d2)

--------------------------------------------------------------------------------
-- | Sample a sigma bond and optional BondingSystem between two atoms.
sampleBond :: (Molecule, Int) -> (AtomId, AtomId) -> Meas (Molecule, Int)
sampleBond (m, sid) (i, j) = do
  includeBond <- sample $ uniformD [True, False]
  if includeBond
    then do
      order <- sample $ uniformD [1,2,3]
      let mSigma = addSigma i j m
          (systems', sid') =
            if order > 1
              then let edge = mkEdge i j
                       bs = mkBondingSystem (2*(order-1)) (S.singleton edge) Nothing
                   in (M.insert (SystemId sid) bs (systems mSigma), sid + 1)
              else (systems mSigma, sid)
      return (mSigma { systems = systems' }, sid')
    else return (m, sid)

--------------------------------------------------------------------------------
-- | Pretty print a list of weights with their indices.
prettyPrintWeights :: [Product (Log Double)] -> String
prettyPrintWeights ws =
  unlines $ zipWith format [1 :: Int ..] ws
  where
    format i w = printf "%4d: %s" i (show (getProduct w))

--------------------------------------------------------------------------------
-- | Main: Validate the observed molecule, then sample from the model only if valid.
--------------------------------------------------------------------------------
main :: IO ()
main = do
  observed <- observedMoleculeIO
  case validateMolecule observed of
    Left errs -> do
      putStrLn "Invalid observed molecule:"
      mapM_ (putStrLn . show) errs
    Right validObserved -> do
      putStrLn "Observed molecule is valid. Proceeding with sampling..."
      samples <- mh 0.1 (moleculeModel validObserved)
      let (molecules, weights) = unzip $ take 2000 $ drop 60000 samples
      putStrLn "Sampled molecules:"
      mapM_ (putStrLn . prettyPrintMolecule) molecules
      putStrLn "Weights:"
      putStrLn $ prettyPrintWeights weights
