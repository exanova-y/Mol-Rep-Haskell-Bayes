

module Constants where 

import Molecule
import qualified Data.Vector as V

-- Takes the bond order and two atomic symbols and gives the equilibrium bond length between them.
equilibriumBondLengths :: Integer -> AtomicSymbol -> AtomicSymbol -> EquilibriumBondLength
equilibriumBondLengths bondOrder symbol1 symbol2 =
    case (symbol1, symbol2) of
        (C, C)   -> Angstrom 1.54
        (C, H)   -> Angstrom 1.09
        (C, O)   -> Angstrom 1.43
        (C, N)   -> Angstrom 1.47
        (C, B)   -> Angstrom 1.55
        (C, Fe)  -> Angstrom 1.84
        (H, C)   -> Angstrom 1.09
        (H, H)   -> Angstrom 0.74
        (H, O)   -> Angstrom 0.96
        (H, N)   -> Angstrom 1.01
        (H, B)   -> Angstrom 1.19
        (H, Fe)  -> Angstrom 1.52
        (O, C)   -> Angstrom 1.43
        (O, H)   -> Angstrom 0.96
        (O, O)   -> Angstrom 1.48
        (O, N)   -> Angstrom 1.40
        (O, B)   -> Angstrom 1.49
        (O, Fe)  -> Angstrom 1.70
        (N, C)   -> Angstrom 1.47
        (N, H)   -> Angstrom 1.01
        (N, O)   -> Angstrom 1.40
        (N, N)   -> Angstrom 1.45
        (N, B)   -> Angstrom 1.55
        (N, Fe)  -> Angstrom 1.76
        (B, C)   -> Angstrom 1.55
        (B, H)   -> Angstrom 1.19
        (B, O)   -> Angstrom 1.49
        (B, N)   -> Angstrom 1.55
        (B, B)   -> Angstrom 1.59
        (B, Fe)  -> Angstrom 2.03
        (Fe, C)  -> Angstrom 1.84
        (Fe, H)  -> Angstrom 1.52
        (Fe, O)  -> Angstrom 1.70
        (Fe, N)  -> Angstrom 1.76
        (Fe, B)  -> Angstrom 2.03
        (Fe, Fe) -> Angstrom 2.48


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
priorAbundances = V.fromList [0.6179,0.3279,0.0378,0.0126,0.0025,0.0013]
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
