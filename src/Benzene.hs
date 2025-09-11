module Benzene where

import Chem.Molecule
import Chem.Molecule.Coordinate (Coordinate(..), mkAngstrom)
import Chem.Dietz
import Constants (elementAttributes, elementShells)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

benzene :: Molecule
benzene = Molecule
  { atoms = M.fromList
      [ (atomID atom, atom)
      | (i,sym,x,y,z) <- atomsData
      , let atom = Atom { atomID = AtomId i
                        , attributes = elementAttributes sym
                        , coordinate = Coordinate (mkAngstrom x) (mkAngstrom y) (mkAngstrom z)
                        , shells     = elementShells sym
                        , formalCharge = 0 }]
  , localBonds = S.fromList (map (uncurry mkEdge') sigmaEdges)
  , systems    = M.fromList
      [ (SystemId 1, mkBondingSystem 6 (S.fromList (map (uncurry mkEdge') ringEdges)) (Just "pi_ring")) ]
  }
  where
    mkEdge' a b = mkEdge (AtomId a) (AtomId b)
    ringEdges = [(1,2),(2,3),(3,4),(4,5),(5,6),(6,1)]
    sigmaEdges = ringEdges ++ [(1,7),(2,8),(3,9),(4,10),(5,11),(6,12)]

atomsData :: [(Integer, AtomicSymbol, Double, Double, Double)]
atomsData =
  [ (1,C,-1.2131,-0.6884,0.0),  (2,C,-1.2028, 0.7064,0.0)
  , (3,C,-0.0103,-1.3948,0.0),  (4,C, 0.0104, 1.3948,0.0)
  , (5,C, 1.2028,-0.7063,0.0),  (6,C, 1.2131, 0.6884,0.0)
  , (7,H,-2.1577,-1.2244,0.0),  (8,H,-2.1393, 1.2564,0.0)
  , (9,H,-0.0184,-2.4809,0.0),  (10,H,0.0184, 2.4808,0.0)
  , (11,H,2.1394,-1.2563,0.0),  (12,H,2.1577, 1.2245,0.0)
  ]
