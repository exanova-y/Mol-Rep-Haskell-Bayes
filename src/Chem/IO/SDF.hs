{-# LANGUAGE OverloadedStrings #-}
module Chem.IO.SDF
  ( readSDF
  , parseSDF
  ) where

import           Control.Monad (void)
import           Data.Void (Void)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read (readMaybe)

import           Chem.Molecule
import           Chem.Molecule.Coordinate (Coordinate(..), mkAngstrom)
import           Chem.Dietz
import           Constants (elementAttributes)
import           Orbital (elementShells)

-- | Parser type on String input.
type Parser = Parsec Void String

-- | Read an SDF file into a 'Molecule'.
readSDF :: FilePath -> IO (Either (ParseErrorBundle String Void) Molecule)
readSDF fp = do
  txt <- readFile fp
  pure (parseSDF txt)

-- | Parse SDF contents into a 'Molecule'.
parseSDF :: String -> Either (ParseErrorBundle String Void) Molecule
parseSDF = runParser sdfFile "SDF"

-- Internal parser for a complete V2000 record.
sdfFile :: Parser Molecule
sdfFile = do
  -- skip header lines
  count 3 (manyTill anySingle eol)
  (nAtoms, nBonds) <- parseCounts
  atoms <- zipWithM parseAtom [1..nAtoms] (replicate nAtoms ())
  bonds <- count nBonds parseBond
  chg   <- concat <$> many (try parseChargeLine <|> otherLine)
  _     <- string "M  END"
  -- consume any trailing data (properties/$$$$ markers)
  manyTill anySingle eof
  let atomMap0 = M.fromList [(atomID a, a) | a <- atoms]
      atomMap  = foldl' applyCharge atomMap0 chg
      mol = Molecule atomMap (S.fromList bonds) M.empty
  pure mol
  where
    applyCharge m (i,q) = M.adjust (\a -> a { formalCharge = q }) i m

-- | Parse the counts line (atom and bond counts).
parseCounts :: Parser (Int, Int)
parseCounts = do
  line <- manyTill anySingle eol
  let ws = words line
  case ws of
    (a:b:_) -> pure (read a, read b)
    _       -> fail "Invalid counts line"

-- | Parse one atom line assigning an index.
parseAtom :: Int -> () -> Parser Atom
parseAtom idx _ = do
  line <- manyTill anySingle eol
  let ws = words line
  case ws of
    (xs:ys:zs:sym:_) ->
      case readMaybe sym of
        Just symbol ->
          let x = read xs; y = read ys; z = read zs
              coord = Coordinate (mkAngstrom x) (mkAngstrom y) (mkAngstrom z)
          in pure Atom { atomID = AtomId (fromIntegral idx)
                        , attributes = elementAttributes symbol
                        , coordinate = coord
                        , shells = elementShells symbol
                        , formalCharge = 0 }
        Nothing -> fail ("Unknown atomic symbol: " ++ sym)
    _ -> fail "Invalid atom line"

-- | Parse a bond line returning an undirected 'Edge'.
parseBond :: Parser Edge
parseBond = do
  line <- manyTill anySingle eol
  let ws = words line
  case ws of
    (a:b:_) ->
      let i = AtomId (read a)
          j = AtomId (read b)
      in pure (mkEdge i j)
    _ -> fail "Invalid bond line"

-- | Parse an 'M  CHG' line producing atom/charge pairs.
parseChargeLine :: Parser [(AtomId, Int)]
parseChargeLine = do
  _ <- string "M  CHG"
  hspace1
  n <- L.decimal
  pairs <- count n $ do
    hspace1
    i <- L.decimal
    hspace1
    q <- L.signed hspace L.decimal
    pure (AtomId (fromIntegral i), q)
  hspace
  eol
  pure pairs

-- | Skip any other property line.
otherLine :: Parser [(AtomId, Int)]
otherLine = do
  notFollowedBy (string "M  END")
  manyTill anySingle eol
  pure []

-- | Convenience: Megaparsec's 'eol'.
eol :: Parser ()
eol = void newline

-- | 'zipWithM' for lists.
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ [] []         = pure []
zipWithM f (x:xs) (y:ys) = (:) <$> f x y <*> zipWithM f xs ys
zipWithM _ _ _           = fail "zipWithM: mismatched lengths"

-- | strict 'foldl'' used locally.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = go z xs
  where
    go acc []     = acc
    go acc (y:ys) = let acc' = f acc y in acc' `seq` go acc' ys
