module Molecules.Methane where 
import Molecule

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
          [ Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond {bondOrder = 1} }
          , Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond {bondOrder = 1} }
          , Bond { connectedAtom = hydrogenAtom3, bondType = CovalentBond {bondOrder = 1} }
          , Bond { connectedAtom = hydrogenAtom4, bondType = CovalentBond {bondOrder = 1} }
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
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }

    hydrogenAtom2 = Atom
      { atomID = 3
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = -0.629118, y = -0.629118, z = 0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }

    hydrogenAtom3 = Atom
      { atomID = 4
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = 0.629118, y = -0.629118, z = -0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }

    hydrogenAtom4 = Atom
      { atomID = 5
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = Coordinate {x = -0.629118, y = 0.629118, z = -0.629118}
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }


-- H2 molecule
h2 :: Atom
h2 = hydrogenAtom1
  where
    hydrogenAtom1 = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.0, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond { bondOrder = 1 } }]
      }
    hydrogenAtom2 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.74, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond { bondOrder = 1 } }]
      }

-- O2 molecule
o2 :: Atom
o2 = oxygenAtom1
  where
    oxygenAtom1 = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = O, atomicNumber = 8, atomicWeight = 15.999 }
      , coordinate = Coordinate { x = 0.0, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom2, bondType = CovalentBond { bondOrder = 2 } }]
      }
    oxygenAtom2 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = O, atomicNumber = 8, atomicWeight = 15.999 }
      , coordinate = Coordinate { x = 1.21, y = 0.0, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom1, bondType = CovalentBond { bondOrder = 2 } }]
      }

-- H2O molecule
h2o :: Atom
h2o = oxygenAtom
  where
    oxygenAtom = Atom
      { atomID = 1
      , atomicAttr = ElementAttributes { symbol = O, atomicNumber = 8, atomicWeight = 15.999 }
      , coordinate = Coordinate { x = 0.0, y = 0.0, z = 0.0 }
      , bondList =
          [ Bond { connectedAtom = hydrogenAtom1, bondType = CovalentBond { bondOrder = 1 } }
          , Bond { connectedAtom = hydrogenAtom2, bondType = CovalentBond { bondOrder = 1 } }
          ]
      }
    hydrogenAtom1 = Atom
      { atomID = 2
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = 0.757, y = 0.586, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom, bondType = CovalentBond { bondOrder = 1 } }]
      }
    hydrogenAtom2 = Atom
      { atomID = 3
      , atomicAttr = ElementAttributes { symbol = H, atomicNumber = 1, atomicWeight = 1.008 }
      , coordinate = Coordinate { x = -0.757, y = 0.586, z = 0.0 }
      , bondList = [Bond { connectedAtom = oxygenAtom, bondType = CovalentBond { bondOrder = 1 } }]
      }