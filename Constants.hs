

module Constants where 

import Molecule
import qualified Data.Vector as V
import LazyPPL

-- Takes the bond order and two atomic symbols and gives the equilibrium bond length between them.
-- Currently this is only working for covalent single bonds.
equilibriumBondLengths :: Integer -> AtomicSymbol -> AtomicSymbol -> EquilibriumBondLength
equilibriumBondLengths bondOrder symbol1 symbol2 =
    case (bondOrder, symbol1, symbol2) of
        (1, C, C)   -> Angstrom 1.54
        (1, C, H)   -> Angstrom 1.09
        (1, C, O)   -> Angstrom 1.43
        (1, C, N)   -> Angstrom 1.47
        (1, C, B)   -> Angstrom 1.55
        (1, C, Fe)  -> Angstrom 1.84
        (1, H, C)   -> Angstrom 1.09
        (1, H, H)   -> Angstrom 0.74
        (1, H, O)   -> Angstrom 0.96
        (1, H, N)   -> Angstrom 1.01
        (1, H, B)   -> Angstrom 1.19
        (1, H, Fe)  -> Angstrom 1.52
        (1, O, C)   -> Angstrom 1.43
        (1, O, H)   -> Angstrom 0.96
        (1, O, O)   -> Angstrom 1.48
        (1, O, N)   -> Angstrom 1.40
        (1, O, B)   -> Angstrom 1.49
        (1, O, Fe)  -> Angstrom 1.70
        (1, N, C)   -> Angstrom 1.47
        (1, N, H)   -> Angstrom 1.01
        (1, N, O)   -> Angstrom 1.40
        (1, N, N)   -> Angstrom 1.45
        (1, N, B)   -> Angstrom 1.55
        (1, N, Fe)  -> Angstrom 1.76
        (1, B, C)   -> Angstrom 1.55
        (1, B, H)   -> Angstrom 1.19
        (1, B, O)   -> Angstrom 1.49
        (1, B, N)   -> Angstrom 1.55
        (1, B, B)   -> Angstrom 1.59
        (1, B, Fe)  -> Angstrom 2.03
        (1, Fe, C)  -> Angstrom 1.84
        (1, Fe, H)  -> Angstrom 1.52
        (1, Fe, O)  -> Angstrom 1.70
        (1, Fe, N)  -> Angstrom 1.76
        (1, Fe, B)  -> Angstrom 2.03
        (1, Fe, Fe) -> Angstrom 2.48
        (2, C, C) -> Angstrom 1.34
        (2, C, H) -> Angstrom 1.06
        (2, C, O) -> Angstrom 1.20
        (2, C, N) -> Angstrom 1.27
        (2, C, B) -> Angstrom 1.37
        (2, C, Fe) -> Angstrom 1.64
        (2, H, C) -> Angstrom 1.06
        (2, H, H) -> Angstrom 0.74
        (2, H, O) -> Angstrom 0.96
        (2, H, N) -> Angstrom 1.01
        (2, H, B) -> Angstrom 1.19
        (2, H, Fe) -> Angstrom 1.52
        (2, O, C) -> Angstrom 1.20
        (2, O, H) -> Angstrom 0.96
        (2, O, O) -> Angstrom 1.21
        (2, O, N) -> Angstrom 1.20
        (2, O, B) -> Angstrom 1.26
        (2, O, Fe) -> Angstrom 1.58
        (2, N, C) -> Angstrom 1.27
        (2, N, H) -> Angstrom 1.01
        (2, N, O) -> Angstrom 1.20
        (2, N, N) -> Angstrom 1.25
        (2, N, B) -> Angstrom 1.33
        (2, N, Fe) -> Angstrom 1.64
        (2, B, C) -> Angstrom 1.37
        (2, B, H) -> Angstrom 1.19
        (2, B, O) -> Angstrom 1.26
        (2, B, N) -> Angstrom 1.33
        (2, B, B) -> Angstrom 1.59
        (2, B, Fe) -> Angstrom 1.89
        (2, Fe, C) -> Angstrom 1.64
        (2, Fe, H) -> Angstrom 1.52
        (2, Fe, O) -> Angstrom 1.58
        (2, Fe, N) -> Angstrom 1.64
        (2, Fe, B) -> Angstrom 1.89
        (2, Fe, Fe) -> Angstrom 2.26
        (3, C, C) -> Angstrom 1.20
        (3, C, H) -> Angstrom 1.06
        (3, C, O) -> Angstrom 1.13
        (3, C, N) -> Angstrom 1.14
        (3, C, B) -> Angstrom 1.19
        (3, C, Fe) -> Angstrom 1.44
        (3, H, C) -> Angstrom 1.06
        (3, H, H) -> Angstrom 0.74
        (3, H, O) -> Angstrom 0.96
        (3, H, N) -> Angstrom 1.01
        (3, H, B) -> Angstrom 1.19
        (3, H, Fe) -> Angstrom 1.52
        (3, O, C) -> Angstrom 1.13
        (3, O, H) -> Angstrom 0.96
        (3, O, O) -> Angstrom 1.21
        (3, O, N) -> Angstrom 1.06
        (3, O, B) -> Angstrom 1.20
        (3, O, Fe) -> Angstrom 1.58
        (3, N, C) -> Angstrom 1.14
        (3, N, H) -> Angstrom 1.01
        (3, N, O) -> Angstrom 1.06
        (3, N, N) -> Angstrom 1.10
        (3, N, B) -> Angstrom 1.20
        (3, N, Fe) -> Angstrom 1.50
        (3, B, C) -> Angstrom 1.19
        (3, B, H) -> Angstrom 1.19
        (3, B, O) -> Angstrom 1.20
        (3, B, N) -> Angstrom 1.20
        (3, B, B) -> Angstrom 1.59
        (3, B, Fe) -> Angstrom 1.89
        (3, Fe, C) -> Angstrom 1.44
        (3, Fe, H) -> Angstrom 1.52
        (3, Fe, O) -> Angstrom 1.58
        (3, Fe, N) -> Angstrom 1.50
        (3, Fe, B) -> Angstrom 1.89
        (3, Fe, Fe) -> Angstrom 2.26
        (_, _, _) -> undefined


-- This takes an atom and calculates how many more bonds it can have at most.
getMaxBonds :: Atom -> Integer
getMaxBonds atom = getMaxBondsSymbol (symbol $ atomicAttr atom) - sum (getBondOrders (bondList atom))

getBondOrders :: [Bond] -> [Integer]
getBondOrders bonds = map extractBondOrder bonds
  where
    extractBondOrder :: Bond -> Integer
    extractBondOrder (Delocalised _ _ _) = undefined
    extractBondOrder (Bond _ (CovalentBond order)) = order
    extractBondOrder (Bond _ HydrogenBond) = 1
    extractBondOrder (Bond _ IonicBond) = 1

getMaxBondsSymbol :: AtomicSymbol -> Integer
getMaxBondsSymbol symbol =
    case symbol of
        O  -> 2
        H  -> 1
        N  -> 3
        C  -> 4
        B  -> 3
        Fe -> 6

-- priorAbundances :: V.Vector Double
-- priorAbundances = V.fromList $ take 6 $ repeat (1.0/6.0)
-- -- O (Oxygen): 0.49
-- -- H (Hydrogen): 0.26
-- -- N (Nitrogen): 0.03
-- -- C (Carbon): 0.01
-- -- B (Boron): 0.002
-- -- Fe (Iron): 0.001

elementAttributes :: AtomicSymbol -> ElementAttributes
elementAttributes O = (ElementAttributes O 8 15.999 )
elementAttributes H = (ElementAttributes H 1 1.008 )
elementAttributes N = (ElementAttributes N 7 14.007)
elementAttributes C = (ElementAttributes C 6 12.011)
elementAttributes B = (ElementAttributes B 5 10.811  )
elementAttributes Fe =( ElementAttributes Fe 26 55.845)
