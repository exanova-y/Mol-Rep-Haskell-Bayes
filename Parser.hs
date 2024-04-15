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

type Parser = Parsec Void String

processSDFDirectory :: FilePath -> IO [(FilePath, ([Atom], Maybe Double))]
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
                return ([], Nothing)
            Right atomsAndDbl -> return atomsAndDbl

processSDFFile :: FilePath -> IO ()
processSDFFile filePath = do
    result <- parseSDFFile filePath
    case result of
        Left err -> putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
        Right atomsAndDbl -> processAtoms filePath atomsAndDbl

parseSDFFile :: FilePath -> IO (Either (ParseErrorBundle String Void) ([Atom], Maybe Double))
parseSDFFile filePath = do
    sdfString <- readFile filePath
    return $ runParser parseSDFContents filePath sdfString

parseSDFContents :: Parser ([Atom], Maybe Double)
parseSDFContents = do
    -- Skip the header lines
    manyTill anySingle (try $ string "\n")
    manyTill anySingle (try $ string "\n")
    manyTill anySingle (try $ string "\n")
    countLine <- manyTill anySingle (try $ string "\n")
    let (atomCount, bondCount, _) = parseCountLine countLine
    atoms <- zipWithM parseAtom [1..atomCount] (replicate atomCount ())
    bondLines <- count bondCount (manyTill anySingle (try $ string "\n"))
    let updatedAtoms = foldl updateAtomBonds atoms bondLines
    -- Parse the "M END" line and any additional lines
    void $ manyTill anySingle (try $ string ">  <logS>\n")
    logSValue <- optional $ do
        logSLine <- manyTill anySingle (try $ string "\n")
        case reads logSLine of
            [(value, "")] -> return value
            _             -> fail "Invalid logS value"
    -- Skip the remaining lines
    manyTill anySingle eof
    return (updatedAtoms, logSValue)

updateAtomBonds :: [Atom] -> String -> [Atom]
updateAtomBonds atoms bondLine =
    case words bondLine of
        (atom1Str:atom2Str:bondOrderStr:_) ->
            let atom1ID = read atom1Str
                atom2ID = read atom2Str
                bondOrder :: Int = read bondOrderStr
                atom1 = atoms !! (atom1ID - 1)
                atom2 = atoms !! (atom2ID - 1)
                bond1 = Bond atom2 CovalentBond
                bond2 = Bond atom1 CovalentBond
                updatedAtom1 = atom1 { bondList = bond1 : bondList atom1 }
                updatedAtom2 = atom2 { bondList = bond2 : bondList atom2 }
                updatedAtoms = updateAtomInList updatedAtom1 $ updateAtomInList updatedAtom2 atoms
            in updatedAtoms
        _ -> error "Invalid bond line format"
  where
    updateAtomInList updatedAtom atoms = map (\atom -> if atomID atom == atomID updatedAtom then updatedAtom else atom) atoms

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
    Atom (fromIntegral $ atomicID) (elementAttributes symbol) coord [] (elementShells symbol)


processAtoms :: FilePath -> ([Atom], Maybe Double) -> IO ()
processAtoms filePath (atoms, logSValue) = do
    putStrLn $ "Parsed molecule from file: " ++ filePath
    case logSValue of
        Just value -> putStrLn $ "LogS value: " ++ show value
        Nothing -> putStrLn "LogS value not found in file."
    putStrLn "Parsed atoms:"
    mapM_ putStrLn (map show atoms)

main :: IO ()
main = do
    let dirPath = "./sdfs/"
    results <- processSDFDirectory dirPath
    let molecules = map (\(_, (atoms, logS)) -> (atoms, logS)) results
    putStrLn $ "Parsed " ++ show (length molecules) ++ " molecules from " ++ show (length results) ++ " files."
    putStrLn "Parsed molecules and logS values:"
    mapM_ (\(atoms, logS) -> do
        putStrLn "Molecule:"
        mapM_ putStrLn (map show atoms)
        putStrLn $ "LogS: " ++ maybe "N/A" show logS
        putStrLn "") molecules














parseDB1Contents :: Parser ([Atom], Maybe Double)
parseDB1Contents = do
    -- Skip the header lines
    void $ manyTill anySingle (try $ string "\n")
    void $ manyTill anySingle (try $ string "\n")
    void $ manyTill anySingle (try $ string "\n")
    countLine <- manyTill anySingle (try $ string "\n")
    let (atomCount, bondCount, _) = parseCountLine countLine
    atoms <- zipWithM parseAtom [1..atomCount] (replicate atomCount ())
    bondLines <- count bondCount (manyTill anySingle (try $ string "\n"))
    let updatedAtoms = foldl updateAtomBonds atoms bondLines
    -- Parse any additional lines until "> <logP>"
    void $ manyTill anySingle (try $ string ">  <logP>")
    -- Parse the logP value
    logPValue <- optional $ do
        void $ manyTill anySingle (try $ string "\n")
        logPLine <- manyTill anySingle (try $ string "\n")
        case runParser parseDouble "" logPLine of
            Left _ -> fail "Invalid logP value"
            Right value -> return value
    -- Skip the remaining lines until "$$$$"
    void $ manyTill anySingle (try $ string "$$$$\n")
    return (updatedAtoms, logPValue)

-- parseDB1File :: FilePath -> IO [(FilePath, ([Atom], Maybe Double))]
-- parseDB1File filePath = do
--     db1String <- readFile filePath
--     let parsedMolecules = runParser (many parseDB1Molecule) filePath db1String
--     case parsedMolecules of
--         Left err -> do
--             putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
--             return []
--         Right molecules -> return molecules

parseDB1File :: FilePath -> IO [(FilePath, ([Atom], Maybe Double))]
parseDB1File filePath = do
    db1ByteString <- BL.readFile filePath
    let db1String = T.unpack (TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict db1ByteString))
    let parsedMolecules = runParser (many parseDB1Molecule) filePath db1String
    case parsedMolecules of
        Left err -> do
            putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
            return []
        Right molecules -> return molecules


-- parseDB1File :: FilePath -> IO [(FilePath, ([Atom], Maybe Double))]
-- parseDB1File filePath = do
--     db1ByteString <- BL.readFile filePath
--     let db1String = T.unpack (TE.decodeUtf8 (BL.toStrict db1ByteString))
--     let parsedMolecules = runParser (many parseDB1Molecule) filePath db1String
--     case parsedMolecules of
--         Left err -> do
--             putStrLn $ "Error parsing file " ++ filePath ++ ": " ++ errorBundlePretty err
--             return []
--         Right molecules -> return molecules


parseDB1Molecule :: Parser (FilePath, ([Atom], Maybe Double))
parseDB1Molecule = do
    (atoms, logPValue) <- parseDB1Contents
    molID <- parseMolID ""
    return (molID, (atoms, logPValue))
  where
    parseMolID :: String -> Parser FilePath
    parseMolID _ = return ""





main2 :: IO ()
main2 = do
    let db1FilePath = "./sdfs/DB1.sdf"
    db1Molecules <- parseDB1File db1FilePath
    putStrLn $ "Parsed " ++ show (length db1Molecules) ++ " molecules from file: " ++ db1FilePath
    -- mapM_ (\(molID, (atoms, logP)) -> do
    --     putStrLn $ "Molecule ID: " ++ molID
    --     putStrLn "Atoms:"
    --     mapM_ putStrLn (map show atoms)
    --     putStrLn $ "LogP: " ++ maybe "N/A" show logP
    --     putStrLn "") db1Molecules