
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

newtype Molecule = Root Atom

data Bond =   Delocalised {delocNum :: Integer, 
                           atoms :: [Atom], 
                           bondType :: BondType}
            | Bond {connectedAtom :: Atom, 
                    bondType :: BondType}

data Atom = Atom {
    atomId                   :: Integer,
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
prettyPrintMolecule (Root atom) = show (symbol (atomicSpec atom))

-- prettyPrintAtom :: Atom -> String 
-- prettyPrintAtom atom = show (symbol (atomicSpec atom)) ++ replicate 10 ' ' ++ ['\n'] ++ map prettyPrintAtom (bondList

-- connectedAtoms :: Atom -> [Atom]
-- conneectedAtoms atom = (bondList atom)

-- -- Example Usage
-- main :: IO ()
-- main = do
--   let exampleAtom = 
--   let molecule = Root exampleAtom
--   putStrLn $ prettyPrintMolecule molecule