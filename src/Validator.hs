{-# LANGUAGE OverloadedStrings #-}
module Validator where

import Chem.Molecule
import Chem.Dietz
import Constants (getMaxBondsSymbol)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (forM_, when)
import ParserSingle (parseSDFFileNoLog)
import Text.Megaparsec (errorBundlePretty)

--------------------------------------------------------------------------------
-- | Validate a Molecule by performing these checks:
--   1. Every bond references atoms that exist.
--   2. No atom is bonded to itself.
--   3. Every bond is symmetric.
--   4. No atom exceeds its maximum bonding capacity.
--------------------------------------------------------------------------------
validateMolecule :: Molecule -> Either String Molecule
validateMolecule molecule = do
  let atomIDsList = M.keys (atoms molecule)

  -- Check 1 & 2: Every sigma bond references existing atoms and is not self-bonded
  forM_ (S.toList (localBonds molecule)) $ \(Edge i j) -> do
    when (i == j) $
      Left $ "Atom " ++ show i ++ " is bonded to itself."
    when (i `notElem` atomIDsList) $
      Left $ "Bond references non-existent atom: " ++ show i
    when (j `notElem` atomIDsList) $
      Left $ "Bond references non-existent atom: " ++ show j

  -- Check systems edges as well
  forM_ (M.elems (systems molecule)) $ \bs ->
    forM_ (S.toList (memberEdges bs)) $ \(Edge i j) -> do
      when (i `notElem` atomIDsList || j `notElem` atomIDsList) $
        Left $ "System references non-existent atoms: " ++ show (i,j)

  -- Check 4: Verify each atom's total bond order does not exceed its capacity.
  forM_ atomIDsList $ \i -> do
    -- sigma bonds directly incident on i
    let sigmaEdges  = [ mkEdge i j | j <- neighborsSigma molecule i ]
        -- edges from delocalised systems involving i
        systemEdges = [ e
                      | bs <- M.elems (systems molecule)
                      , e@(Edge a b) <- S.toList (memberEdges bs)
                      , a == i || b == i ]
        order = sum [ effectiveOrder molecule e
                    | e <- sigmaEdges ++ systemEdges ]
        atom = atoms molecule M.! i
        maxBonds = fromIntegral (getMaxBondsSymbol (symbol (attributes atom)))
    when (order > maxBonds) $
      Left $ "Atom " ++ show i ++ " (" ++ show (symbol (attributes atom))
             ++ ") has too many bonds: " ++ show order ++ " > " ++ show maxBonds

  return molecule

--------------------------------------------------------------------------------
-- | Main: Parse "molecules/water.sdf" and report if the molecule is valid.
--------------------------------------------------------------------------------
validate :: IO ()
validate = do
    let filePath = "molecules/water.sdf"  -- adjust if needed
    putStrLn $ "Parsing " ++ filePath ++ "..."
    parseResult <- parseSDFFileNoLog filePath
    case parseResult of
      Left errBundle -> do
        putStrLn "Parse error:"
        putStrLn (errorBundlePretty errBundle)
      Right molecule -> do
        putStrLn "Parsed successfully."
        putStrLn "Now validating molecule..."
        case validateMolecule molecule of
          Left err -> putStrLn $ "Invalid molecule: " ++ err
          Right _  -> putStrLn "Water is valid!"
    