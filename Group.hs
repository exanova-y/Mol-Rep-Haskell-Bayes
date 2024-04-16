
module Group where 
import Molecule 
import Coordinate
import Molecules.Methane
import Data.List (transpose)

data MoleculeRotation = MoleculeRotation Molecule Coordinate Double deriving (Show)

class Group g where
  mul :: g -> g -> g -- Group operation: combining two operations
  inv :: g -> g -- Inverse of an operation
  e :: g -> g -- Identity operation

instance Group MoleculeRotation where
  mul (MoleculeRotation mol1 axis1 angle1) (MoleculeRotation _ axis2 angle2) =
    combineRotations mol1 axis1 angle1 axis2 angle2
  inv (MoleculeRotation mol axis angle) = MoleculeRotation mol axis (-angle)
  e (MoleculeRotation mol axis angle) = MoleculeRotation mol (Coordinate 0 0 0) 0 -- Identity rotation, angle 0 along any axis

-- Function to combine rotations for molecules
combineRotations :: Molecule -> Coordinate -> Double -> Coordinate -> Double -> MoleculeRotation
combineRotations mol axis1 angle1 axis2 angle2 = MoleculeRotation resultMol resultAxis resultAngle
  where
    resultMol = rotateMoleculeAroundAxis (rotateMoleculeAroundAxis mol axis1 angle1) axis2 angle2
    resultAxis = addCoordinates axis1 axis2
    resultAngle = angle1 + angle2

-- Helper function to add two coordinates
addCoordinates :: Coordinate -> Coordinate -> Coordinate
addCoordinates (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  Coordinate (x1 + x2) (y1 + y2) (z1 + z2)

-- Helper function to get the axis of rotation between two atoms
getRotationAxis :: Molecule -> Integer -> Integer -> Maybe Coordinate
getRotationAxis mol atomId1 atomId2 = do
  atom1 <- findAtom mol atomId1
  atom2 <- findAtom mol atomId2
  let coord1 = coordinate atom1
      coord2 = coordinate atom2
      dx = x coord2 - x coord1
      dy = y coord2 - y coord1
      dz = z coord2 - z coord1
      magnitude = sqrt (dx^2 + dy^2 + dz^2)
  if magnitude == 0
    then Nothing
    else Just (Coordinate (dx / magnitude) (dy / magnitude) (dz / magnitude))

rotateMoleculeAroundAxis :: Molecule -> Coordinate -> Double -> Molecule
rotateMoleculeAroundAxis molecule axis angle = molecule { atoms = map (rotateAtomAroundAxis axis angle) (atoms molecule) }

rotateAtomAroundAxis :: Coordinate -> Double -> Atom -> Atom
rotateAtomAroundAxis axis angle atom = atom { coordinate = rotateCoordinateAroundAxis axis angle (coordinate atom) }

rotateCoordinateAroundAxis :: Coordinate -> Double -> Coordinate -> Coordinate
rotateCoordinateAroundAxis (Coordinate ax ay az) angle (Coordinate cx cy cz) =
  let radians = angle * pi / 180
      cosAngle = cos radians
      sinAngle = sin radians
      rotationMatrix = [ [cosAngle + ax^2 * (1 - cosAngle), ax * ay * (1 - cosAngle) - az * sinAngle, ax * az * (1 - cosAngle) + ay * sinAngle],
                         [ay * ax * (1 - cosAngle) + az * sinAngle, cosAngle + ay^2 * (1 - cosAngle), ay * az * (1 - cosAngle) - ax * sinAngle],
                         [az * ax * (1 - cosAngle) - ay * sinAngle, az * ay * (1 - cosAngle) + ax * sinAngle, cosAngle + az^2 * (1 - cosAngle)] ]
      [cx', cy', cz'] = multiplyMatrixVector rotationMatrix [cx, cy, cz]
  in Coordinate cx' cy' cz'

multiplyMatrixVector :: [[Double]] -> [Double] -> [Double]
multiplyMatrixVector matrix vector = map (sum . zipWith (*) vector) (transpose matrix)

angleBetween :: Coordinate -> Coordinate -> Double
angleBetween (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  let dotProduct = x1 * x2 + y1 * y2 + z1 * z2
      magnitude1 = sqrt (x1^2 + y1^2 + z1^2)
      magnitude2 = sqrt (x2^2 + y2^2 + z2^2)
      cosAngle = dotProduct / (magnitude1 * magnitude2)
  in acos cosAngle * 180 / pi

angles :: IO ()
angles = do
  let carbonCoord = coordinate (atoms methane !! 0)
      hydrogenCoords = map coordinate (drop 1 (atoms methane))
      angles = [angleBetwH2H3, angleBetwH2H4, angleBetwH2H5, angleBetwH3H4, angleBetwH3H5, angleBetwH4H5]
        where
          h2 = hydrogenCoords !! 0
          h3 = hydrogenCoords !! 1
          h4 = hydrogenCoords !! 2
          h5 = hydrogenCoords !! 3
          v2 = Coordinate (x h2 - x carbonCoord) (y h2 - y carbonCoord) (z h2 - z carbonCoord)
          v3 = Coordinate (x h3 - x carbonCoord) (y h3 - y carbonCoord) (z h3 - z carbonCoord)
          v4 = Coordinate (x h4 - x carbonCoord) (y h4 - y carbonCoord) (z h4 - z carbonCoord)
          v5 = Coordinate (x h5 - x carbonCoord) (y h5 - y carbonCoord) (z h5 - z carbonCoord)
          angleBetwH2H3 = angleBetween v2 v3
          angleBetwH2H4 = angleBetween v2 v4
          angleBetwH2H5 = angleBetween v2 v5
          angleBetwH3H4 = angleBetween v3 v4
          angleBetwH3H5 = angleBetween v3 v5
          angleBetwH4H5 = angleBetween v4 v5
  putStrLn "Angles between C-H bonds in methane:"
  mapM_ (putStrLn . (\angle -> "Angle: " ++ show angle)) angles




-- Example application
main :: IO ()
main = do
  let carbonId = 1
      hydrogenId = 2
      rotationAxis = getRotationAxis methane carbonId hydrogenId
  case rotationAxis of
    Just axis -> do
      let rot1 =  MoleculeRotation methane axis (2 * pi / 3) -- 120 degree rotation around the C-H axis
          rotatedMethane = rotateMoleculeAroundAxis methane axis (2 * pi / 3)
          identityRotation = mul rot1 (inv rot1)
      putStrLn "Original methane:"
      putStrLn (prettyPrintMolecule methane)
      putStrLn "Rotated methane:"
      putStrLn (prettyPrintMolecule rotatedMethane)
      putStrLn "Identity rotation:"
      case identityRotation of
        MoleculeRotation mol _ _ -> putStrLn (prettyPrintMolecule mol)
    Nothing -> putStrLn "Invalid atom IDs for rotation axis"