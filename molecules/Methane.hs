module Molecules.Methane where 
import Molecule
import Coordinate
import Orbital

-- Methane molecule (CH4)
methane :: Atom
methane = carbonAtom
  where
    carbonAtom = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes
          { symbol = C
          , atomicNumber = 6
          , atomicWeight = 12.0107
          }
      , coordinate = Coordinate {x = 0.0, y = 0.0, z = 0.0}
      , bondList =
          [ Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom3, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom4, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 0.8164966 0.4082483 0.4082483)), Orbital Py 1 (Just (Coordinate (-0.4082483) 0.8164966 (-0.4082483))), Orbital Pz 1 (Just (Coordinate (-0.4082483) (-0.4082483) 0.8164966))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }

    hydrogenAtom1 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = 0.629118, y = 0.629118, z = 0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }

    hydrogenAtom2 = Atom
      { atomID = 3
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = -0.629118, y = -0.629118, z = 0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }

    hydrogenAtom3 = Atom
      { atomID = 4
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = 0.629118, y = -0.629118, z = -0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }

    hydrogenAtom4 = Atom
      { atomID = 5
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = -0.629118, y = 0.629118, z = -0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }

-- Hydrogen molecule (H2)
h2 :: Atom
h2 = hydrogenAtom1
  where
    hydrogenAtom1 = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.0, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom2 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.74, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }

-- Oxygen molecule (O2)
o2 :: Atom
o2 = oxygenAtom1
  where
    oxygenAtom1 = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = O, atomicNumber = 8, atomicWeight = 15.999 }
      , coordinate = Coordinate { x = 0.0, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom2, bondType = CovalentBond }]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    oxygenAtom2 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = O, atomicNumber = 8, atomicWeight = 15.999 }
      , coordinate = Coordinate { x = 1.21, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom1, bondType = CovalentBond }]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 1 0 0)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate 0 0 1))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }

-- Water molecule (H2O)
h2o :: Atom
h2o = oxygenAtom
  where
    oxygenAtom = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = O, atomicNumber = 8, atomicWeight = 15.999 }
      , coordinate = Coordinate { x = 0.0, y = 0.0, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 2 (Just (Coordinate 0.9578263 0 0.2873479)), Orbital Py 1 (Just (Coordinate 0 1 0)), Orbital Pz 1 (Just (Coordinate (-0.2873479) 0 0.9578263))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    hydrogenAtom1 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.757, y = 0.586, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom2 = Atom
      { atomID = 3
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = -0.757, y = 0.586, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }


-- Benzene molecule (C6H6)
benzene :: Atom
benzene = carbonAtom1
  where
    carbonAtom1 = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = C, atomicNumber = 6, atomicWeight = 12.0107 }
      , coordinate = Coordinate { x = 0.0, y = 1.3948, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = carbonAtom2, bondType = CovalentBond }
          , Bond { connectedAtom = carbonAtom6, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 0 1 0)), Orbital Py 1 (Just (Coordinate (-0.5) 0 (sqrt 3 / 2)))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    carbonAtom2 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = C, atomicNumber = 6, atomicWeight = 12.0107 }
      , coordinate = Coordinate { x = 1.2079, y = 0.6974, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = carbonAtom1, bondType = CovalentBond }
          , Bond { connectedAtom = carbonAtom3, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate (sqrt 3 / 2) 0.5 0)), Orbital Py 1 (Just (Coordinate 0 1 0))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    carbonAtom3 = Atom
      { atomID = 3
      , atomicAttr = ElementAttributes { symbol = C, atomicNumber = 6, atomicWeight = 12.0107 }
      , coordinate = Coordinate { x = 1.2079, y = -0.6974, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = carbonAtom2, bondType = CovalentBond }
          , Bond { connectedAtom = carbonAtom4, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom3, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate (sqrt 3 / 2) (-0.5) 0)), Orbital Py 1 (Just (Coordinate 0 1 0))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    carbonAtom4 = Atom
      { atomID = 4
      , atomicAttr = ElementAttributes { symbol = C, atomicNumber = 6, atomicWeight = 12.0107 }
      , coordinate = Coordinate { x = 0.0, y = -1.3948, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = carbonAtom3, bondType = CovalentBond }
          , Bond { connectedAtom = carbonAtom5, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom4, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate 0 (-1) 0)), Orbital Py 1 (Just (Coordinate 0.5 0 (sqrt 3 / 2)))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    carbonAtom5 = Atom
      { atomID = 5
      , atomicAttr = ElementAttributes { symbol = C, atomicNumber = 6, atomicWeight = 12.0107 }
      , coordinate = Coordinate { x = -1.2079, y = -0.6974, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = carbonAtom4, bondType = CovalentBond }
          , Bond { connectedAtom = carbonAtom6, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom5, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate (-sqrt 3 / 2) (-0.5) 0)), Orbital Py 1 (Just (Coordinate 0 1 0))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }
    carbonAtom6 = Atom
      { atomID = 6
      , atomicAttr = ElementAttributes { symbol = C, atomicNumber = 6, atomicWeight = 12.0107 }
      , coordinate = Coordinate { x = -1.2079, y = 0.6974, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = carbonAtom5, bondType = CovalentBond }
          , Bond { connectedAtom = carbonAtom1, bondType = CovalentBond }
          , Bond { connectedAtom = hydrogenAtom6, bondType = CovalentBond }
          ]
      , shells =
          [ Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }
          , Shell { principalQuantumNumber = 2, sSubShell = Just (SubShell [Orbital S 2 Nothing]), pSubShell = Just (SubShell [Orbital Px 1 (Just (Coordinate (-sqrt 3 / 2) 0.5 0)), Orbital Py 1 (Just (Coordinate 0 1 0))]), dSubShell = Nothing, fSubShell = Nothing }
          ]
      }

    hydrogenAtom1 = Atom
      { atomID = 7
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.0, y = 2.4732, z = 0.0 }
      , bondList = [Bond { connectedAtom = carbonAtom1, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom2 = Atom
      { atomID = 8
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 2.1431, y = 1.2366, z = 0.0 }
      , bondList = [Bond { connectedAtom = carbonAtom2, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom3 = Atom
      { atomID = 9
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 2.1431, y = -1.2366, z = 0.0 }
      , bondList = [Bond { connectedAtom = carbonAtom3, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom4 = Atom
      { atomID = 10
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.0, y = -2.4732, z = 0.0 }
      , bondList = [Bond { connectedAtom = carbonAtom4, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom5 = Atom
      { atomID = 11
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = -2.1431, y = -1.2366, z = 0.0 }
      , bondList = [Bond { connectedAtom = carbonAtom5, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }
    hydrogenAtom6 = Atom
      { atomID = 12
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = -2.1431, y = 1.2366, z = 0.0 }
      , bondList = [Bond { connectedAtom = carbonAtom6, bondType = CovalentBond }]
      , shells = [Shell { principalQuantumNumber = 1, sSubShell = Just (SubShell [Orbital S 1 Nothing]), pSubShell = Nothing, dSubShell = Nothing, fSubShell = Nothing }]
      }