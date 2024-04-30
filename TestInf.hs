import Molecule
import LazyPPL
import Distr
import Control.Monad
import Coordinate
import Extra
import Constants
import Molecules.Methane
import qualified Data.Map as M

-- Observed molecule (e.g., H2O)
observedMolecule :: Molecule
observedMolecule = h2o

-- A generative model for a molecule
moleculeModel :: Meas Molecule
moleculeModel = do
  let numAtoms = 3
  atoms <- replicateM numAtoms $ do
    symbol <- sample $ uniformD [C, N, O, H]
    coord <- sampleCoordinate
    let attr = elementAttributes symbol
    let shellsVar = elementShells symbol
    return $ Atom { atomID = 0, atomicAttr = attr, coordinate = coord, shells = shellsVar }
  let molecule = Molecule { atoms = atoms, bonds = undefined }

  -- Score the model based on the distance to the observed molecule
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

-- Example usage
main :: IO ()
main = do
  samples <- mh 0.1 moleculeModel
  let (molecules, weights) = unzip $ take 1000 $ drop 1000 samples
  putStrLn $ "Sampled molecules: " ++ show molecules
  putStrLn $ "Weights: " ++ show weights