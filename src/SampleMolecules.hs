module SampleMolecules
  ( hydrogen
  , oxygen
  , water
  , methane
  ) where

import Chem.Molecule
import Chem.Molecule.Coordinate (Coordinate(..), mkAngstrom)
import Chem.Dietz (AtomId(..), mkEdge)
import Constants (elementAttributes, elementShells)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | A very small molecule: H2
hydrogen :: Molecule
hydrogen = Molecule
  { atoms = M.fromList
      [ (AtomId 1, hAtom 1 (Coordinate (mkAngstrom 0)    (mkAngstrom 0) (mkAngstrom 0)))
      , (AtomId 2, hAtom 2 (Coordinate (mkAngstrom 0.74) (mkAngstrom 0) (mkAngstrom 0)))
      ]
  , localBonds = S.singleton (mkEdge (AtomId 1) (AtomId 2))
  , systems = M.empty
  }
  where
    hAtom i coord = Atom
      { atomID = AtomId i
      , attributes = elementAttributes H
      , coordinate = coord
      , shells = elementShells H
      , formalCharge = 0
      }

-- | Molecular oxygen: O2
oxygen :: Molecule
oxygen = Molecule
  { atoms = M.fromList
      [ (AtomId 1, oAtom 1 (Coordinate (mkAngstrom 0)    (mkAngstrom 0) (mkAngstrom 0)))
      , (AtomId 2, oAtom 2 (Coordinate (mkAngstrom 1.21) (mkAngstrom 0) (mkAngstrom 0)))
      ]
  , localBonds = S.singleton (mkEdge (AtomId 1) (AtomId 2))
  , systems = M.empty
  }
  where
    oAtom i coord = Atom
      { atomID = AtomId i
      , attributes = elementAttributes O
      , coordinate = coord
      , shells = elementShells O
      , formalCharge = 0
      }

-- | Water molecule H2O
water :: Molecule
water = Molecule
  { atoms = M.fromList
      [ (AtomId 1, oAtom)
      , (AtomId 2, hAtom 2 (Coordinate (mkAngstrom 0.96)  (mkAngstrom 0)    (mkAngstrom 0)))
      , (AtomId 3, hAtom 3 (Coordinate (mkAngstrom (-0.32)) (mkAngstrom 0.90) (mkAngstrom 0)))
      ]
  , localBonds = S.fromList [mkEdge (AtomId 1) (AtomId 2), mkEdge (AtomId 1) (AtomId 3)]
  , systems = M.empty
  }
  where
    oAtom = Atom
      { atomID = AtomId 1
      , attributes = elementAttributes O
      , coordinate = Coordinate (mkAngstrom 0) (mkAngstrom 0) (mkAngstrom 0)
      , shells = elementShells O
      , formalCharge = 0
      }
    hAtom i coord = Atom
      { atomID = AtomId i
      , attributes = elementAttributes H
      , coordinate = coord
      , shells = elementShells H
      , formalCharge = 0
      }

-- | Methane molecule CH4
methane :: Molecule
methane = Molecule
  { atoms = M.fromList
      [ (AtomId 1, cAtom)
      , (AtomId 2, hAtom 2 (Coordinate (mkAngstrom 1.09)  (mkAngstrom 0)    (mkAngstrom 0)))
      , (AtomId 3, hAtom 3 (Coordinate (mkAngstrom (-1.09)) (mkAngstrom 0)    (mkAngstrom 0)))
      , (AtomId 4, hAtom 4 (Coordinate (mkAngstrom 0) (mkAngstrom 1.09) (mkAngstrom 0)))
      , (AtomId 5, hAtom 5 (Coordinate (mkAngstrom 0) (mkAngstrom (-1.09)) (mkAngstrom 0)))
      ]
  , localBonds = S.fromList
      [ mkEdge (AtomId 1) (AtomId 2)
      , mkEdge (AtomId 1) (AtomId 3)
      , mkEdge (AtomId 1) (AtomId 4)
      , mkEdge (AtomId 1) (AtomId 5)
      ]
  , systems = M.empty
  }
  where
    cAtom = Atom
      { atomID = AtomId 1
      , attributes = elementAttributes C
      , coordinate = Coordinate (mkAngstrom 0) (mkAngstrom 0) (mkAngstrom 0)
      , shells = elementShells C
      , formalCharge = 0
      }
    hAtom i coord = Atom
      { atomID = AtomId i
      , attributes = elementAttributes H
      , coordinate = coord
      , shells = elementShells H
      , formalCharge = 0
      }
