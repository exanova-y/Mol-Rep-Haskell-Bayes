

module Constants where 

import Molecule
import qualified Data.Vector as V

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
        (_, _, _)   -> undefined


getMaxBonds :: AtomicSymbol -> Int
getMaxBonds symbol =
    case symbol of
        O  -> 2
        H  -> 1
        N  -> 3
        C  -> 4
        B  -> 3
        Fe -> 3

priorAbundances :: V.Vector Double
priorAbundances = V.fromList $ take 6 $ repeat (1.0/6.0)
-- O (Oxygen): 0.49
-- H (Hydrogen): 0.26
-- N (Nitrogen): 0.03
-- C (Carbon): 0.01
-- B (Boron): 0.002
-- Fe (Iron): 0.001

elementAttributes :: AtomicSymbol -> ElementAttributes
elementAttributes O = ElementAttributes O 8 15.999 
elementAttributes H = ElementAttributes H 1 1.008 
elementAttributes N = ElementAttributes N 7 14.007
elementAttributes C = ElementAttributes C 6 12.011
elementAttributes B = ElementAttributes B 5 10.811  
elementAttributes Fe = ElementAttributes Fe 26 55.845
