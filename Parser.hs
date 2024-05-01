module Parser where
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Text.Megaparsec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Text.Megaparsec.Char
import Data.Void
import System.Directory
import System.FilePath
import Control.Monad
import Molecule
import Constants
import Orbital
import Coordinate
import qualified Data.Map as M

type Parser = Parsec Void String

processSDFDirectory :: FilePath -> IO [(FilePath, (Molecule, Double))]
processSDFDirectory dirPath = do
    contents <- listDirectory dirPath
    let sdfFiles = filter (\file -> takeExtension file == ".sdf") contents
    results <- mapM processSDFFile' sdfFiles
    return $ zip sdfFiles results
  where
    processSDFFile' file = do
        let filePath = dirPath </> file
        result <- parseSDFFile filePath
        case result of
            Left err -> do
                putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
                return (Molecule [] M.empty, 0.0)
            Right moleculeAndDbl -> return moleculeAndDbl

processSDFFile :: FilePath -> IO ()
processSDFFile filePath = do
    result <- parseSDFFile filePath
    case result of
        Left err -> putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
        Right moleculeAndDbl -> processAtoms filePath moleculeAndDbl




parseSDFFile :: FilePath -> IO (Either (ParseErrorBundle String Void) (Molecule, Double))
parseSDFFile filePath = do
    sdfString <- readFile filePath
    return $ runParser parseSDFContents filePath sdfString











parseSDFContents :: Parser (Molecule, Double)
parseSDFContents = do
    -- Skip the header lines
    manyTill anySingle (try $ string "\n")
    manyTill anySingle (try $ string "\n")
    manyTill anySingle (try $ string "\n")
    countLine <- manyTill anySingle (try $ string "\n")
    let (atomCount, bondCount, _) = parseCountLine countLine
    atoms <- zipWithM parseAtom [1..atomCount] (replicate atomCount ())
    bondLines <- count bondCount (manyTill anySingle (try $ string "\n"))
    let bondMatrix = buildBondMatrix atomCount bondLines

    -- Parse the "M END" line and any additional lines
    void $ manyTill anySingle (try $ string ">  <logS>\n")

    -- Parse the logS value
    logSValue <- try $ do
        logSLine <- manyTill anySingle (try $ string "\n")
        case reads logSLine of
            [(value, "")] -> return value
            _ -> fail "Invalid logS value"

    -- Skip the remaining lines
    manyTill anySingle eof
    return (Molecule atoms bondMatrix, logSValue)

buildBondMatrix :: Int -> [String] -> M.Map (Integer, Integer) BondType
buildBondMatrix atomCount bondLines =
    M.fromList $ getSymmetricBonds $ map parseBondLine bondLines
  where
    parseBondLine bondLine =
        case words bondLine of
            (atom1Str:atom2Str:bondOrderStr:_) ->
                let atom1ID = read atom1Str
                    atom2ID = read atom2Str
                    bondOrder = read bondOrderStr
                    bondType = case bondOrder of
                        1 -> CovalentBond {delocNum = 2, atomIDs = Nothing}
                        2 -> CovalentBond {delocNum = 4, atomIDs = Nothing}
                        3 -> CovalentBond {delocNum = 6, atomIDs = Nothing}
                        _ -> error "Invalid bond order"
                in ((atom1ID, atom2ID), bondType)
            _ -> error "Invalid bond line format"



parseCharge :: Parser Int
parseCharge = do
    sign <- optional (char '-')
    charge <- some digitChar
    return $ case sign of
        Just '-' -> negate (read charge)
        _ -> read charge

parseCountLine :: String -> (Int, Int, [Int])
parseCountLine line =
    case words line of
        (atomCount : bondCount : rest) -> (read atomCount, read bondCount, map read rest)
        _ -> error "Invalid count line format"

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
    charge <- parseCharge 

    manyTill anySingle (try $ string "\n")
    return $ makeAtom atomicID (symbol, Coordinate x y z)
    
parseDouble :: Parser Double
parseDouble = do
    sign <- optional (char '-')
    integral <- some digitChar
    fractional <- option "" (char '.' >> some digitChar)
    let num = read $ integral ++ "." ++ fractional
    return $ case sign of
        Just '-' -> -num
        _ -> num

parseAtomicSymbol :: Parser AtomicSymbol
parseAtomicSymbol = do
    symbol <- some letterChar
    space
    many digitChar
    return $ case symbol of
        "C" -> C
        "H" -> H
        "O" -> O
        "N" -> N
        "B" -> B
        "Fe" -> Fe
        "Cl" -> Cl
        "S" -> S
        "F" -> F
        "Br" -> Br
        "P" -> P
        "I" -> I
        _ -> error $ "Unknown atomic symbol: " ++ symbol

makeAtom :: Int -> (AtomicSymbol, Coordinate) -> Atom
makeAtom atomicID (symbol, coord) =
    Atom (fromIntegral $ atomicID) (elementAttributes symbol) coord (elementShells symbol)

processAtoms :: FilePath -> (Molecule, Double) -> IO ()
processAtoms filePath (molecule, logSValue) = do
    putStrLn $ "Parsed molecule from file: " ++ filePath
    putStrLn $ "LogS value: " ++ show logSValue
    putStrLn "Parsed atoms:"
    mapM_ putStrLn (map show (atoms molecule))

test :: IO ()
test = do
    let dirPath = "./logs/"
    results <- processSDFDirectory dirPath
    let molecules = map (\(_, (molecule, logS)) -> (molecule, logS)) results
    putStrLn $ "Parsed " ++ show (length molecules) ++ " molecules from " ++ show (length results) ++ " files."
    putStrLn "Parsed molecules and logS values:"
    mapM_ (\(molecule, logS) -> do
        putStrLn $ prettyPrintMolecule molecule
        putStrLn $ "LogS: " ++ show logS
        putStrLn "") molecules







parseDB1Contents :: Parser (Molecule, Double)
parseDB1Contents = do
    -- Skip the header lines
    void $ manyTill anySingle (try $ string "\n")
    void $ manyTill anySingle (try $ string "\n")
    void $ manyTill anySingle (try $ string "\n")
    countLine <- manyTill anySingle (try $ string "\n")
    let (atomCount, bondCount, _) = parseCountLine countLine
    atoms <- zipWithM parseAtom [1..atomCount] (replicate atomCount ())
    bondLines <- count bondCount (manyTill anySingle (try $ string "\n"))
    let bondMatrix = buildBondMatrix atomCount bondLines

    -- Parse any additional lines until "> <logP>"
    void $ manyTill anySingle (try $ string ">  <logP>")

    -- Parse the logP value
    logPValue <- try $ do
        void $ manyTill anySingle (try $ string "\n")
        logPLine <- manyTill anySingle (try $ string "\n")
        case runParser parseDouble "" logPLine of
            Left _ -> fail "Invalid logP value"
            Right value -> return value

    -- Skip the remaining lines until "$$$$"
    void $ manyTill anySingle (try $ string "$$$$\n")
    return (Molecule atoms bondMatrix, logPValue)

parseDB1File :: FilePath -> IO [(Molecule, Double)]
parseDB1File filePath = do
    db1ByteString <- BL.readFile filePath
    let db1String = T.unpack (TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict db1ByteString))
    let parsedMolecules = runParser (many parseDB1Molecule) filePath db1String
    case parsedMolecules of
        Left err -> do
            putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
            return []
        Right molecules -> return molecules

parseDB1Molecule :: Parser (Molecule, Double)
parseDB1Molecule = do
    (molecule, logPValue) <- parseDB1Contents
    return (molecule, logPValue)

test2 :: IO ()
test2 = do
    let db1FilePath = "./logp/DB1.sdf"
    db1Molecules <- parseDB1File db1FilePath
    putStrLn $ "Parsed " ++ show (length db1Molecules) ++ " molecules from file: " ++ db1FilePath
    mapM_ (\(molecule, logP) -> do
        putStrLn $ prettyPrintMolecule molecule
        putStrLn $ "LogP: " ++ show logP
        putStrLn "") db1Molecules


