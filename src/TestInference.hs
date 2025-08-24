{-# LANGUAGE RecordWildCards #-}
module TestInference where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (tails)
import Molecule
import LazyPPL
import Parser
import Distr
import Control.Monad
import Coordinate
import ExtraF
import Constants
import Validator
import Text.Printf (printf)


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
  -- Generate atoms without unique IDs.
  atomsUnnumbered <- replicateM numAtoms $ do
    symbol <- sample $ uniformD [C, N, O, H]
    coord  <- sampleCoordinate
    let attr      = elementAttributes symbol
        shellsVar = elementShells symbol
    return $ Atom { atomID = 0, atomicAttr = attr, coordinate = coord, shells = shellsVar }

  -- Assign unique IDs (1,2,3) to the atoms.
  let atoms = zipWith (\i atom -> atom { atomID = i }) [1..] atomsUnnumbered
      atomIDs = map atomID atoms

  -- Get all unique pairs of atoms (for 3 atoms: (1,2), (1,3), (2,3))
  let possiblePairs = [(i, j) | (i:rest) <- tails atomIDs, j <- rest]

  -- For each pair, randomly decide whether to include a bond.
  bondsMaybe <- mapM (\pair -> do
                         includeBond <- sample $ uniformD [True, False]
                         if includeBond
                           then do
                             bondOrder <- sample $ uniformD [1, 2, 3]
                             let bondType = case bondOrder of
                                               1 -> Bond { delocNum = 2, atomIDs = Nothing }
                                               2 -> Bond { delocNum = 4, atomIDs = Nothing }
                                               3 -> Bond { delocNum = 6, atomIDs = Nothing }
                                               _ -> error "Invalid bond order"
                             return $ Just (pair, bondType)
                           else return Nothing)
                      possiblePairs
  let bondsList = catMaybes bondsMaybe
      bondsMap  = M.fromList (getSymmetricBonds bondsList)  -- Ensure symmetry

  -- Create the molecule with atoms and bonds.
  let molecule = Molecule { atoms = atoms, bonds = bondsMap }

  -- Score the model based on the distance to the observed molecule.
  let distance = hausdorffDistance molecule observed
  score $ normalPdf 0 1 distance

  return molecule

-- | Randomly generate a bond graph for a molecule with n atoms.
--   For each unique pair (i,j) (with i < j), we randomly decide whether
--   to include a bond. If a bond is included, we sample its bond order from [1,2,3]
--   and then create a bond with a corresponding 'delocNum'.
randomBonds :: Int -> Meas (M.Map (Integer, Integer) BondType)
randomBonds n = do
  let pairs = [ (i, j) | i <- [1..fromIntegral n], j <- [i+1 .. fromIntegral n] ]
  bondList <- forM pairs $ \(i, j) -> do
      include <- sample $ uniformD [False, True]
      if include
         then do
           bondOrder <- sample $ uniformD [1,2,3]
           let bondType = case bondOrder of
                   1 -> Bond { delocNum = 2, atomIDs = Nothing }
                   2 -> Bond { delocNum = 4, atomIDs = Nothing }
                   3 -> Bond { delocNum = 6, atomIDs = Nothing }
                   _ -> error "Invalid bond order"
           return $ Just ((i, j), bondType)
         else return Nothing
  let bondsList = catMaybes bondList
  return $ M.fromList (getSymmetricBonds bondsList)


--------------------------------------------------------------------------------
-- Sample a coordinate from a normal distribution.
sampleCoordinate :: Meas Coordinate
sampleCoordinate = do
  x <- sample $ normal 0 1
  y <- sample $ normal 0 1
  z <- sample $ normal 0 1
  return $ Coordinate x y z

--------------------------------------------------------------------------------
-- Calculate the Euclidean distance between two coordinates.
euclideanDistance :: Coordinate -> Coordinate -> Double
euclideanDistance (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  sqrt $ (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

--------------------------------------------------------------------------------
-- Compute the Hausdorff distance between two molecules.
hausdorffDistance :: Molecule -> Molecule -> Double
hausdorffDistance mol1 mol2 =
  let coords1 = map coordinate (atoms mol1)
      coords2 = map coordinate (atoms mol2)
      -- For each atom in mol1, find the distance to the closest atom in mol2.
      d1 = [minimum [euclideanDistance c1 c2 | c2 <- coords2] | c1 <- coords1]
      -- For each atom in mol2, find the distance to the closest atom in mol1.
      d2 = [minimum [euclideanDistance c2 c1 | c1 <- coords1] | c2 <- coords2]
  in max (maximum d1) (maximum d2)

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
      let (molecules, weights) = unzip $ take 1000 $ drop 1000 samples
      putStrLn "Sampled molecules:"
      mapM_ (putStrLn . prettyPrintMolecule) molecules
      putStrLn $ "Weights: " ++ show weights
