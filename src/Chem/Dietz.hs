{-# LANGUAGE DeriveGeneric #-}

-- | Dietz: constitution-level primitives.
--   - AtomId, SystemId: stable identifiers
--   - Edge: canonical UNDIRECTED atom–atom edge (unordered pair)
--   - BondingSystem: one electron pool 's' spread over a set of edges 'E'
--   See: Dietz, "Yet Another Representation of Molecular Structure", JCICS (1995).

module Chem.Dietz
  ( AtomId(..), SystemId(..)
  , Edge(..), mkEdge, atomsOfEdge
  , BondingSystem(..), mkBondingSystem
  ) where

import           GHC.Generics (Generic)
import           Data.Set     (Set)
import qualified Data.Set     as S

-- Stable identifiers
newtype AtomId   = AtomId Integer deriving (Eq, Ord, Show, Read, Generic)
newtype SystemId = SystemId Int    deriving (Eq, Ord, Show, Read, Generic)

-- | Canonical undirected edge (store as ordered pair with i <= j).
data Edge = Edge AtomId AtomId
  deriving (Eq, Ord, Show, Read, Generic)

mkEdge :: AtomId -> AtomId -> Edge
mkEdge a b = if a <= b then Edge a b else Edge b a

atomsOfEdge :: Edge -> (AtomId, AtomId)
atomsOfEdge (Edge i j) = (i, j)

-- | One Dietz bonding system: s shared electrons over memberEdges E.
--   'memberAtoms' is cached for fast validation/queries.
data BondingSystem = BondingSystem
  { sharedElectrons :: Int          -- ^ s >= 0
  , memberAtoms     :: Set AtomId   -- ^ derived from edges (cache)
  , memberEdges     :: Set Edge     -- ^ set of undirected edges E
  , tag             :: Maybe String -- ^ optional label (e.g., "pi_ring")
  } deriving (Eq, Show, Read, Generic)

-- | Smart constructor: derive atom scope from the edges.
mkBondingSystem :: Int -> Set Edge -> Maybe String -> BondingSystem
mkBondingSystem s es lbl =
  let scope = S.fromList [ v | e <- S.toList es, v <- let (i,j) = atomsOfEdge e in [i,j] ]
  in BondingSystem s scope es lbl
