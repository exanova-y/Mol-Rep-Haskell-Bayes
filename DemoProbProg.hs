module DemoProbProg where 
import Control.Monad
import LazyPPL
import Coordinate
import Molecule
import Constants
import Distr
import ExtraF
import Data.List
import Data.Maybe
import qualified Data.Map as M

-- Observed molecule (e.g., H2O)
observedMolecule :: Molecule
observedMolecule = undefined -- Omitted for brevity

-- A generative model for a molecule
moleculeModel :: Meas Molecule
moleculeModel = do
  let numAtoms = 3
  -- Generate atoms without unique IDs.
  atomsUnnumbered <- replicateM numAtoms $ do
    symbol <- sample $ uniformD [C, N, O, H]
    coord  <- sampleCoordinate
    let attr      = elementAttributes symbol
        shellsVar = elementShells symbol
    return $ Atom { atomID = 0, atomicAttr = attr, coordinate = coord, shells = shellsVar }
  -- Assign unique IDs (1,2,...,numAtoms) to the atoms.
  let atoms = zipWith (\i atom -> atom { atomID = i }) [1..] atomsUnnumbered
  -- Generate a random bond graph for the given number of atoms.
  bondsMap <- randomBonds numAtoms
  let molecule = Molecule { atoms = atoms, bonds = bondsMap }

  -- Score the model based on the distance to the observed molecule.
  let distance = distMolecule molecule observedMolecule
  score $ normalPdf 0 1 distance

  return molecule

-- Sample a coordinate from a normal distribution
sampleCoordinate :: Meas Coordinate
sampleCoordinate = do
  x <- sample $ normal 0 1
  y <- sample $ normal 0 1
  z <- sample $ normal 0 1
  return $ Coordinate x y z

-- Calculate the Euclidean distance between two coordinates
euclideanDistance :: Coordinate -> Coordinate -> Double
euclideanDistance (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  sqrt $ (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

-- Calculate the distance between two molecules
distMolecule :: Molecule -> Molecule -> Double
distMolecule mol1 mol2 =
  let coords1 = map coordinate (atoms mol1)
      coords2 = map coordinate (atoms mol2)
      distances = [euclideanDistance c1 c2 | c1 <- coords1, c2 <- coords2]
  in minimum distances

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

-- Example usage
main :: IO ()
main = do
  samples <- mh 0.1 moleculeModel
  let (molecules, weights) = unzip $ take 1000 $ drop 1000 samples
  putStrLn $ "Sampled molecules: " ++ show molecules
  putStrLn $ "Weights: " ++ show weights