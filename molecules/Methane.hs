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
      , coordinate = (0, 0, 0)
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
      , coordinate = (0.629118, 0.629118, 0.629118)
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }

    hydrogenAtom2 = Atom
      { atomID = 3
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = (-0.629118, -0.629118, 0.629118)
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }

    hydrogenAtom3 = Atom
      { atomID = 4
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = (0.629118, -0.629118, -0.629118)
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }

    hydrogenAtom4 = Atom
      { atomID = 5
      , atomicAttr = ElementAttributes
          { symbol = H
          , atomicNumber = 1
          , atomicWeight = 1.008
          }
      , coordinate = (-0.629118, 0.629118, -0.629118)
      , bondList = [Bond { connectedAtom = carbonAtom, bondType = CovalentBond {bondOrder = 1} }]
      }