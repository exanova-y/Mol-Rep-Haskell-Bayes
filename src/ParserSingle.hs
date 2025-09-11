{-# LANGUAGE OverloadedStrings #-}
module ParserSingle where

import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad
import Chem.Molecule
import Chem.Dietz
import Constants
import Orbital
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

-- | The parser type alias.
type Parser = Parsec Void String

--------------------------------------------------------------------------------
-- SDF Parser (No logS/logP)
--------------------------------------------------------------------------------

-- | Parse the contents of an SDF file and return a Molecule.
--   This parser skips header lines, reads the count line, atoms, bonds,
--   and then stops at the "M  END" marker.
parseSDFContentsNoLog :: Parser Molecule
parseSDFContentsNoLog = do
    -- Skip the first 3 header lines.
    void $ count 3 (manyTill anySingle newline)
    -- Read the count line (e.g., "  3  2  0  0  0               999 V2000")
    countLine <- manyTill anySingle newline
    let (atomCount, bondCount, _) = parseCountLine countLine
    -- Parse the atoms.
    atomsList <- zipWithM parseAtom [1..atomCount] (replicate atomCount ())
    -- Parse the bond lines and insert as \963 edges.
    bondLines <- count bondCount (manyTill anySingle newline)
    let atoms = M.fromList [ (atomID a, a) | a <- atomsList ]
        molecule0 = Molecule atoms S.empty M.empty
        moleculeWithBonds = applyBondLines molecule0 bondLines
    -- Skip until the "M  END" marker.
    void $ manyTill anySingle (try (string "M  END"))
    -- Skip the remainder of the file.
    void $ manyTill anySingle eof
    return moleculeWithBonds

-- | Parse an SDF file from the given file path and return a Molecule.
parseSDFFileNoLog :: FilePath -> IO (Either (ParseErrorBundle String Void) Molecule)
parseSDFFileNoLog filePath = do
    sdfString <- readFile filePath
    return $ runParser parseSDFContentsNoLog filePath sdfString

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Insert bonds described by V2000 lines into a molecule via 'addSigma'.
applyBondLines :: Molecule -> [String] -> Molecule
applyBondLines = foldl' addBond
  where
    addBond m line = case words line of
      (a1:a2:_) ->
        let i = AtomId (read a1)
            j = AtomId (read a2)
        in addSigma i j m
      _ -> m

-- | Parse a count line into atom count, bond count, and any extra numbers.
--   For example, the line "  3  2  0  0  0               999 V2000" will return (3,2,[]).
parseCountLine :: String -> (Int, Int, [Int])
parseCountLine line =
    case words line of
        (atomCount : bondCount : _rest) -> (read atomCount, read bondCount, [])
        _ -> error "Invalid count line format"

-- | Parse an atom from the SDF file.
parseAtom :: Int -> () -> Parser Atom
parseAtom atomicID _ = do
    space
    x <- parseDouble
    space
    y <- parseDouble
    space
    z <- parseDouble
    space
    symbol <- parseAtomicSymbol
    space
    _ <- parseCharge  -- charge is parsed but ignored
    manyTill anySingle newline
    return $ makeAtom atomicID (symbol, Coordinate (mkAngstrom x) (mkAngstrom y) (mkAngstrom z))

-- | Parse a double number.
parseDouble :: Parser Double
parseDouble = do
    sign <- optional (char '-')
    integral <- some digitChar
    fractional <- option "" (char '.' >> some digitChar)
    let num = read (integral ++ if null fractional then "" else "." ++ fractional)
    return $ case sign of
        Just '-' -> -num
        _ -> num

-- | Parse an atomic symbol.
parseAtomicSymbol :: Parser AtomicSymbol
parseAtomicSymbol = do
    symStr <- some letterChar
    space
    many digitChar  -- skip any trailing digits
    return $ case symStr of
        "C"  -> C
        "H"  -> H
        "O"  -> O
        "N"  -> N
        "B"  -> B
        "Fe" -> Fe
        "Cl" -> Cl
        "S"  -> S
        "F"  -> F
        "Br" -> Br
        "P"  -> P
        "I"  -> I
        _    -> error $ "Unknown atomic symbol: " ++ symStr

-- | Parse a charge. (We ignore the parsed value.)
parseCharge :: Parser Int
parseCharge = do
    sign <- optional (char '-')
    digits <- some digitChar
    return $ case sign of
        Just '-' -> negate (read digits)
        _        -> read digits

-- | Create an Atom given an ID, atomic symbol, and coordinate.
makeAtom :: Int -> (AtomicSymbol, Coordinate) -> Atom
makeAtom atomicID (symbol, coord) =
    Atom { atomID = AtomId (fromIntegral atomicID)
         , attributes = elementAttributes symbol
         , coordinate = coord
         , shells = elementShells symbol
         , formalCharge = 0 }

