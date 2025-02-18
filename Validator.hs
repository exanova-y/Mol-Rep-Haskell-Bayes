{-# LANGUAGE OverloadedStrings #-}
module Validator where

import Molecule
import Constants (getMaxBondsSymbol)
import qualified Data.Map as M
import Control.Monad (forM_, when)
import ParserTwo (parseSDFFileNoLog)
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
  let atomIDsList = map atomID (atoms molecule)
  
  -- Check 1: Every bond must reference atoms that exist.
  forM_ (M.toList (bonds molecule)) $ \((i, j), _) -> do
    when (i `notElem` atomIDsList) $
      Left $ "Bond references non-existent atom: " ++ show i
    when (j `notElem` atomIDsList) $
      Left $ "Bond references non-existent atom: " ++ show j
  
  -- Check 2: No atom should be bonded to itself.
  forM_ (M.toList (bonds molecule)) $ \((i, j), _) ->
    when (i == j) $
      Left $ "Atom " ++ show i ++ " is bonded to itself."
  
  -- Check 3: Every bond must be symmetric.
  forM_ (M.toList (bonds molecule)) $ \((i, j), bond) -> do
    case M.lookup (j, i) (bonds molecule) of
      Nothing ->
        Left $ "Bond between atoms " ++ show i ++ " and " ++ show j ++ " is not symmetric."
      Just bond' ->
        when (bond /= bond') $
          Left $ "Bond between atoms " ++ show i ++ " and " ++ show j ++ " is not symmetric."
  
  -- Check 4: Verify each atom's total bond order does not exceed its capacity.
  forM_ (atoms molecule) $ \atom -> do
    let totalBondOrder = sum [ delocNum bond
                             | ((i, _), bond) <- M.toList (bonds molecule)
                             , i == atomID atom
                             ] `div` 2
        maxBonds = getMaxBondsSymbol (symbol (atomicAttr atom))
    when (totalBondOrder > maxBonds) $
      Left $ "Atom " ++ show (atomID atom) ++ " (" ++ show (symbol (atomicAttr atom))
             ++ ") has too many bonds: " ++ show totalBondOrder ++ " > " ++ show maxBonds
  
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
    