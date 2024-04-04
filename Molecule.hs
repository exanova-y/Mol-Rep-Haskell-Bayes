
{-# LANGUAGE RankNTypes #-}

module Molecule where

import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List 
import Numeric.Log( Log( Exp ), ln )
import Data.Time.Format.ISO8601 (yearFormat)

newtype Molecule = Root Atom

data Bond =   Delocalised {delocNum :: Integer, 
                           atoms :: [Atom], 
                           bondType :: BondType}
            | Bond {connectedAtom :: Atom, 
                    bondType :: BondType}

data Atom = Atom {
    atomID                   :: Integer,
    atomicSpec               :: ElementAttributes, 
    coordinate               :: (Double, Double, Double),
    bondList                 :: [Bond]
  }

data BondType = HydrogenBond 
              | CovalentBond {bondOrder :: Integer}  
              | IonicBond deriving (Eq, Read, Show)

data AtomicSymbol = O | H | N | C | B | Fe deriving (Eq, Read, Show)

data ElementAttributes = ElementAttributes
  { symbol :: AtomicSymbol,
    atomicNumber :: Integer,
    atomicWeight :: Double
  } deriving (Eq, Read, Show)

newtype EquilibriumBondLength = Angstrom Double deriving (Read, Show, Eq)

prettyPrintMolecule :: Molecule -> String
prettyPrintMolecule (Root atom) = (prettyPrintAtom 0 atom)

prettyPrintAtom :: Int -> Atom -> String
prettyPrintAtom n atom = replicate (n*5) ' ' ++ show  (symbol $ atomicSpec atom) ++ show (atomID atom) ++ "\n" ++ concat (map (prettyPrintAtom (n+1)) (getChildren atom))

getChildren :: Atom -> [Atom]
getChildren atom = concatMap extractAtoms (bondList atom)
  where
    extractAtoms (Delocalised _ atoms _) = atoms
    extractAtoms (Bond connectedAtom _)  = [connectedAtom]








-- dist :: Molecule -> Molecule -> Double 
-- dist mol mol2 = x - yearFormat
--   where x = 