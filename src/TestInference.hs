{-# LANGUAGE RecordWildCards #-}
module TestInference where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import Data.List (tails)
import Chem.Molecule
import Chem.Dietz
import LazyPPL
import Parser
import Distr
import Control.Monad
import ExtraF
import Constants
import Validator
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
      atomIDs = map atomID atomsList
  let possiblePairs = [(i, j) | (i:rest) <- tails atomIDs, j <- rest]
  bondsMaybe <- mapM (\pair@(i,j) -> do
                         includeBond <- sample $ uniformD [True, False]
                         if includeBond
                           then do
                             order <- sample $ uniformD [1,2,3]
                             let edge = mkEdge i j
                             return $ Just (edge, order)
                           else return Nothing)
                      possiblePairs
  let edgesWithOrder = catMaybes bondsMaybe
      localB = S.fromList [e | (e, _) <- edgesWithOrder]
      systems' = M.fromList
        [ (SystemId idx, mkBondingSystem (2*(o-1)) (S.singleton e) Nothing)
        | ((e,o), idx) <- zip edgesWithOrder [1..], o > 1]
      atoms = M.fromList [ (atomID a, a) | a <- atomsList ]
      molecule = Molecule { atoms = atoms, localBonds = localB, systems = systems' }

  -- Score the model based on the distance to the observed molecule.
  let distance = hausdorffDistance molecule observed
  score $ normalPdf 0 1 distance

  return molecule

-- | Randomly generate a bond graph for a molecule with n atoms.
--   For each unique pair (i,j) (with i < j), we randomly decide whether
--   to include a bond. If a bond is included, we sample its bond order from [1,2,3]
--   and then create a bond with a corresponding 'delocNum'.
-- legacy helper removed


--------------------------------------------------------------------------------
-- Sample a coordinate from a normal distribution.
sampleCoordinate :: Meas Coordinate
sampleCoordinate = do
  x <- sample $ normal 0 1
  y <- sample $ normal 0 1
  z <- sample $ normal 0 1
  return $ Coordinate (Angstrom x) (Angstrom y) (Angstrom z)

--------------------------------------------------------------------------------
-- Calculate the Euclidean distance between two coordinates.
euclideanDistance :: Coordinate -> Coordinate -> Double
euclideanDistance (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  let dx = unAngstrom x1 - unAngstrom x2
      dy = unAngstrom y1 - unAngstrom y2
      dz = unAngstrom z1 - unAngstrom z2
  in sqrt (dx*dx + dy*dy + dz*dz)

--------------------------------------------------------------------------------
-- Compute the Hausdorff distance between two molecules.
hausdorffDistance :: Molecule -> Molecule -> Double
hausdorffDistance mol1 mol2 =
  let coords1 = map coordinate (M.elems (atoms mol1))
      coords2 = map coordinate (M.elems (atoms mol2))
      -- For each atom in mol1, find the distance to the closest atom in mol2.
      d1 = [minimum [euclideanDistance c1 c2 | c2 <- coords2] | c1 <- coords1]
      -- For each atom in mol2, find the distance to the closest atom in mol1.
      d2 = [minimum [euclideanDistance c2 c1 | c1 <- coords1] | c2 <- coords2]
  in max (maximum d1) (maximum d2)

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
    Left err -> putStrLn $ "Invalid observed molecule: " ++ err
    Right validObserved -> do
      putStrLn "Observed molecule is valid. Proceeding with sampling..."
      samples <- mh 0.1 (moleculeModel validObserved)
      let (molecules, weights) = unzip $ take 2000 $ drop 60000 samples
      putStrLn "Sampled molecules:"
      mapM_ (putStrLn . prettyPrintMolecule) molecules
      putStrLn "Weights:"
      putStrLn $ prettyPrintWeights weights
