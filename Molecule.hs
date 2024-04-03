import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List (sort)
import Numeric.Log( Log( Exp ), ln )

data Molecule = Molecule {atoms :: [Atom], bonds :: [Bond]} deriving (Eq, Read, Show)

data Bond = Delocalised Integer [(Atom, Atom, BondType, EquilibriumBondLength)]
          | Bond (Atom, Atom, BondType, EquilibriumBondLength)
          deriving (Eq, Read, Show)

newtype InductiveMolecule = InitialAtom ExtendedAtom

data ExtendedAtom = EAtom {atom :: Atom, bondList :: [InductiveBond]}

data InductiveBond = DelocalisedI Integer ([ExtendedAtom], BondType, EquilibriumBondLength)
                    | InductiveBond (ExtendedAtom, BondType, EquilibriumBondLength)

data Atom = Atom {
    atomId                   :: Integer,
    atomicSpec               :: ElementAttributes, 
    coordinate               :: (Double, Double, Double)
  } deriving (Eq, Read, Show)

data BondType = HydrogenBond 
              | CovalentBond {bondOrder :: Integer}  
              | IonicBond deriving (Eq, Read, Show)

newtype Angstrom = Angstrom Double deriving (Read, Show, Eq)

type EquilibriumBondLength = Angstrom

data AtomicSymbol = O | H | N | C | P | S | Cl | B | Fe deriving (Eq, Read, Show)

data ElementAttributes = ElementAttributes
  { symbol :: AtomicSymbol,
    atomicNumber :: Integer,
    atomicWeight :: Double
  } deriving (Eq, Read, Show)

testOrbital :: MonadSample m => Double -> m Double
testOrbital threshold = do
  sample <- normal 0 1
  return sample

abundances :: V.Vector Double
abundances = V.fromList [0.49, 0.26, 0.03, 0.01, 0.008, 0.006, 0.004, 0.002, 0.001]
-- O (Oxygen): 0.49
-- H (Hydrogen): 0.26
-- N (Nitrogen): 0.03
-- C (Carbon): 0.01
-- P (Phosphorus): 0.008
-- S (Sulfur): 0.006
-- Cl (Chlorine): 0.004
-- B (Boron): 0.002
-- Fe (Iron): 0.001

elementAttributes :: AtomicSymbol -> ElementAttributes
elementAttributes O = ElementAttributes O 8 15.999 
elementAttributes H = ElementAttributes H 1 1.008 
elementAttributes N = ElementAttributes N 7 14.007
elementAttributes C = ElementAttributes C 6 12.011
elementAttributes P = ElementAttributes P 15 30.974
elementAttributes S = ElementAttributes S 16 32.065
elementAttributes Cl = ElementAttributes Cl 17 35.453
elementAttributes B = ElementAttributes B 5 10.811  
elementAttributes Fe = ElementAttributes Fe 26 55.845

priorElementAttributes :: MonadInfer m => m ElementAttributes
priorElementAttributes = do
  index <- categorical abundances
  return $ case index of
    0 -> elementAttributes O
    1 -> elementAttributes H
    2 -> elementAttributes N
    3 -> elementAttributes C
    4 -> elementAttributes P
    5 -> elementAttributes S
    6 -> elementAttributes Cl
    7 -> elementAttributes B
    8 -> elementAttributes Fe
    _ -> elementAttributes C