
import Constants 
import Molecule 
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List (sort)
import Numeric.Log( Log( Exp ), ln )

initMolecule :: MonadSample m => m (Molecule, [Integer])
initMolecule = do
    (root, ids) <- initRoot [1..]
    (updatedRoot, remainingIDs) <- appendAtoms 4 root ids
    return (Root updatedRoot, remainingIDs)

initRoot :: MonadSample m => [Integer] -> m (Atom, [Integer])
initRoot listIDs@(nextID:restIDs) = do 
    firstAtomSpec <- categElementAttributes priorAbundances
    initPosition <- liftM3 (,,) (normal 0.0 1.0) (normal 0.0 1.0) (normal 0.0 1.0)
    initCoordinate <- sampleNewPosition initPosition (Angstrom 0.5)
    return (Atom {atomID = nextID, atomicSpec = firstAtomSpec, coordinate = initCoordinate, bondList = []}, restIDs)
initRoot [] = undefined


-- This takes the current atom position and returns a random position
-- from a sphere with radius bondlength away from the atom.
sampleNewPosition :: MonadSample m => (Double, Double, Double) -> EquilibriumBondLength -> m (Double, Double, Double)
sampleNewPosition (currX, currY, currZ) (Angstrom bondLength) = do
    theta <- uniform 0 (2 * pi)
    u <- uniform (-1) 1
    let phi = acos u
    let x = bondLength * sin phi * cos theta
    let y = bondLength * sin phi * sin theta
    let z = bondLength * cos phi
    return (currX + x, currY + y, currZ + z)

-- This takes an atom, a stream of IDs and returns the same atom with a bond to 
-- a new atom, a boolean value indicating whether the max number of bonds has been 
-- reached for that atom and the remainder of the stream of IDs. 
appendAtom :: MonadSample m => Atom -> [Integer] -> m (Atom, Bool, [Integer])
appendAtom currentAtom listIDs@(nextID:restIDs) = do
    let currentSymbol = symbol (atomicSpec currentAtom)
    let currentBondList = bondList currentAtom
    if length currentBondList == getMaxBonds currentSymbol
        then return (currentAtom, False, listIDs)
        else do
            nextAtomSpec <- categElementAttributes priorAbundances
            let nextSymbol = symbol nextAtomSpec
            let bondLength = equilibriumBondLengths 1 currentSymbol nextSymbol
            nextAtomCoordinate <- sampleNewPosition (coordinate currentAtom) bondLength
            let newAtom = Atom { atomID = nextID
                               , atomicSpec = nextAtomSpec
                               , coordinate = nextAtomCoordinate
                               , bondList = []
                               }
            let updatedBondList = currentBondList ++ [Bond {connectedAtom = newAtom, bondType = CovalentBond {bondOrder = 1}}]
            return (currentAtom { bondList = updatedBondList }, True, restIDs)
appendAtom currentAtom [] = undefined

appendAtoms :: MonadSample m => Int -> Atom -> [Integer] -> m (Atom, [Integer])
appendAtoms 0 atom ids = return (atom, ids)
appendAtoms n atom ids = do
    (updatedAtom, success, remainingIDs) <- appendAtom atom ids
    if success
        then appendAtoms (n - 1) updatedAtom remainingIDs
    else return (updatedAtom, remainingIDs)




testOrbital :: MonadSample m => Double -> m Double
testOrbital threshold = do
  sample <- normal 0 1
  return sample


categElementAttributes :: MonadSample m => V.Vector Double -> m ElementAttributes
categElementAttributes abundances = do
  index <- categorical abundances
  return $ case index of
    0 -> elementAttributes O
    1 -> elementAttributes H
    2 -> elementAttributes N
    3 -> elementAttributes C
    4 -> elementAttributes B
    5 -> elementAttributes Fe
    _ -> elementAttributes C



prettyPrintMolecule :: Molecule -> String
prettyPrintMolecule (Root atom) = (prettyPrintAtom 0 atom)

prettyPrintAtom :: Int -> Atom -> String
prettyPrintAtom n atom = replicate (n*5) ' ' ++ show (symbol (atomicSpec atom)) ++ "\n" ++ concat (map (prettyPrintAtom (n+1)) (getChildren atom))

getChildren :: Atom -> [Atom]
getChildren atom = concatMap extractAtoms (bondList atom)
  where
    extractAtoms (Delocalised _ atoms _) = atoms
    extractAtoms (Bond connectedAtom _)  = [connectedAtom]






-- Example Usage
main :: IO ()
main = do
    sampleIO (initMolecule >>= \(mol, _) -> return (prettyPrintMolecule mol)) >>= putStrLn









-- appendRing :: MonadSample m => Atom -> m (Atom, Bool)
-- appendRing currentAtom listIDs@(nextID:restIDs) = do
--     ringSize <- uniformD [3..8]



-- determineSiteOfRing :: 

-- -- buildMolecule :: MonadSample m => () -> m InductiveMolecule
-- -- buildMolecule = do 
-- --   ea <- priorElementAttributes 
-- --   listOfIds <- [1..]

-- addRing :: Atom -> [Integer]
-- addRing currentAtom listIDs@(nextID:restIDs) = do
--   ringSize <- uniformD [3..8]

