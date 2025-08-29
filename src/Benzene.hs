module Benzene where

import Molecule
import Coordinate
import Constants (elementAttributes, elementShells)
import qualified Data.Map as M

benzene :: Molecule
benzene = Molecule
  { atoms =
      [ Atom { atomID = i
             , atomicAttr = elementAttributes sym
             , coordinate = Coordinate x y z
             , shells     = elementShells sym
             }
      | (i,sym,x,y,z) <- atomsData
      ]
  , bonds = M.fromList $ getSymmetricBonds
      [ ((1,2), ring), ((2,3), ring), ((3,4), ring)
      , ((4,5), ring), ((5,6), ring), ((6,1), ring)
      , ((1,7), single), ((2,8), single), ((3,9), single)
      , ((4,10), single), ((5,11), single), ((6,12), single)
      ]
  }
  where
    ring   = Bond { delocNum = 6, atomIDs = Just [1..6] }
    single = Bond { delocNum = 2, atomIDs = Nothing }

atomsData :: [(Integer, AtomicSymbol, Double, Double, Double)]
atomsData =
  [ (1,C,-1.2131,-0.6884,0.0),  (2,C,-1.2028, 0.7064,0.0)
  , (3,C,-0.0103,-1.3948,0.0),  (4,C, 0.0104, 1.3948,0.0)
  , (5,C, 1.2028,-0.7063,0.0),  (6,C, 1.2131, 0.6884,0.0)
  , (7,H,-2.1577,-1.2244,0.0),  (8,H,-2.1393, 1.2564,0.0)
  , (9,H,-0.0184,-2.4809,0.0),  (10,H,0.0184, 2.4808,0.0)
  , (11,H,2.1394,-1.2563,0.0),  (12,H,2.1577, 1.2245,0.0)
  ]
