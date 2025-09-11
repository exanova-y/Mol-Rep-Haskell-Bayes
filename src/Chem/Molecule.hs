{-# LANGUAGE DeriveGeneric #-}

-- | Molecule ADT built on Dietz constitution:
--   - atoms      : Map AtomId Atom (element data, charge, coordinates)
--   - localBonds : \963 adjacency as undirected edges (2e -> 1e per endpoint)
--   - systems    : Dietz bonding systems (delocalized/multicenter pools)
--   Distances/coordinates are stored in Angstroms.

module Chem.Molecule
  ( -- * Core types
    AtomicSymbol(..), ElementAttributes(..)
  , Angstrom(..), mkAngstrom, unAngstrom
  , Coordinate(..)
  , Shells(..)
  , Atom(..)
  , Molecule(..)
    -- * Helpers
  , addSigma
  , distanceAngstrom
  ) where

import           GHC.Generics (Generic)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Chem.Dietz

-- ===== Element + units =====

data AtomicSymbol = H | C | N | O | S | P | F | Cl | Br | I | Fe | B
  deriving (Eq, Ord, Show, Read, Generic)

data ElementAttributes = ElementAttributes
  { symbol       :: AtomicSymbol
  , atomicNumber :: Int
  , atomicWeight :: Double
  } deriving (Eq, Show, Read, Generic)

-- Distances in Angstroms
newtype Angstrom = Angstrom Double
  deriving (Eq, Ord, Show, Read, Generic)

mkAngstrom :: Double -> Angstrom
mkAngstrom = Angstrom

unAngstrom :: Angstrom -> Double
unAngstrom (Angstrom d) = d

-- Cartesian coordinates in Angstroms
data Coordinate = Coordinate
  { x :: Angstrom, y :: Angstrom, z :: Angstrom
  } deriving (Eq, Show, Read, Generic)

-- Placeholder (keep/extend your existing electronic structure ADT if desired)
data Shells = Shells
  deriving (Eq, Show, Read, Generic)

-- ===== Atoms =====

data Atom = Atom
  { atomID       :: AtomId
  , attributes   :: ElementAttributes
  , coordinate   :: Coordinate
  , shells       :: Shells
  , formalCharge :: Int       -- explicit charge; do NOT overload “unshared e−”
  } deriving (Eq, Show, Read, Generic)

-- ===== Molecule (Dietz + \963) =====

data Molecule = Molecule
  { atoms      :: Map AtomId Atom           -- V
  , localBonds :: Set Edge                  -- \963 adjacency (2e bonds)
  , systems    :: Map SystemId BondingSystem -- B (each system is (s, E))
  } deriving (Eq, Show, Read, Generic)

-- ===== Small helpers =====

-- | Insert a \963 bond between two atoms (undirected).
addSigma :: AtomId -> AtomId -> Molecule -> Molecule
addSigma i j m = m { localBonds = S.insert (mkEdge i j) (localBonds m) }

-- | Euclidean distance between two atoms, returned in Angstroms.
distanceAngstrom :: Atom -> Atom -> Angstrom
distanceAngstrom a b =
  let Coordinate x1 y1 z1 = coordinate a
      Coordinate x2 y2 z2 = coordinate b
      dx = unAngstrom x1 - unAngstrom x2
      dy = unAngstrom y1 - unAngstrom y2
      dz = unAngstrom z1 - unAngstrom z2
  in Angstrom (sqrt (dx*dx + dy*dy + dz*dz))
