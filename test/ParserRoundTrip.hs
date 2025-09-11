{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Chem.IO.SDF (readSDF, parseSDF)
import Chem.Molecule
import Chem.Molecule.Coordinate (Coordinate(..), unAngstrom)
import Chem.Dietz
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "SDF round-trip" $ do
  it "preserves atom count and sigma adjacency" $ do
    parsed <- readSDF "molecules/benzene.sdf"
    case parsed of
      Left err -> expectationFailure (errorBundlePretty err)
      Right mol -> do
        let sdf = moleculeToSDF mol
        case parseSDF sdf of
          Left err -> expectationFailure (errorBundlePretty err)
          Right mol' -> do
            M.size (atoms mol') `shouldBe` M.size (atoms mol)
            localBonds mol' `shouldBe` localBonds mol

-- | Minimal V2000 writer sufficient for round-trip testing.
moleculeToSDF :: Molecule -> String
moleculeToSDF m = unlines $ header ++ atomLines ++ bondLines ++ ["M  END"]
  where
    nAtoms = M.size (atoms m)
    nBonds = S.size (localBonds m)
    header = ["", "", "", countLine]
    countLine = unwords [show nAtoms, show nBonds, "0 0 0 0 0 0 0 0 0 0 0 0"]
    atomLines = map formatAtom (map snd (M.toAscList (atoms m)))
    formatAtom a =
      let Coordinate x y z = coordinate a
          sym = show (symbol (attributes a))
      in unwords [show (unAngstrom x), show (unAngstrom y), show (unAngstrom z), sym, "0"]
    bondLines = map formatBond (S.toList (localBonds m))
    formatBond (Edge (AtomId i) (AtomId j)) = unwords [show i, show j, "1"]
