{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- The Secret Santa assigner
-- Reads a list of friends from the standard input (or a file) and renders to a
-- specified output file (or "output.pdf" if not specified) all assignments of
-- gift givers and their corresponding receivers encoded in a QR-Code.

module Main where

import Control.Monad (liftM)
import Data.List (intersperse, isSuffixOf)
import Data.Map (Map, singleton, elems, (!), insert)
import Data.Maybe ( fromMaybe )
import Data.QRCode
import Data.Word (Word8)
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude
import System.Console.GetOpt hiding (Option)
import qualified System.Console.GetOpt as GetOpt
import System.Environment (getArgs, getProgName)
import System.Random

-- The QR-Code is a matrix of 0/1 bytes
type QRCodeMatrix = [[Word8]]

-- Return a random permutation from the list. Implementation based on
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen (k:ks) =
    toElems $ foldl step (initial k gen) $ zip [1..] ks where
        toElems (x, y) = (elems x, y)
        initial x g = (singleton 0 x, g)
        step :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
        step (m, g) (i, x) = ((insert j x . insert i (m ! j)) m, g') where
            (j, g') = randomR (0, i) g

-- From a list of friends names, return a random assignment of gift giver and
-- receiver, as tuples, given that the receiver name will be encoded as a
-- QR-Code. The assignment is such that all giver-receiver are in a single
-- loop.
assign :: RandomGen g => g -> [String] -> IO ([(String, QRCodeMatrix)], g)
assign g friends@(_:_:_) = do
    qrCodes <- mapM qrEncode $ tail shuffled ++ shuffled
    return $ shuffle g' $ zip shuffled qrCodes where
        (shuffled, g') = shuffle g friends
        qrEncode s = liftM toMatrix $
                     encodeString s Nothing QR_ECLEVEL_L QR_MODE_EIGHT True
assign _ _ = error "Please provide at least two names"

-- Partition a list in intervals, returning a grid
toGrid :: Int -> [a] -> [[a]]
toGrid _ [] = [[]]
toGrid r l = a:toGrid r b where
    (a, b) = splitAt r l

-- Renders a list of tuples of giver-name and qr-encoded-receiver-name into a
-- diagram layout as a grid
friendsDiagram :: Int -> [(String, QRCodeMatrix)] -> Diagram Cairo R2
friendsDiagram rowSize l = vcat $ intersperse hSep $
                           map formatRow $ toGrid rowSize $ map cell l where
    formatRow row = centerX $ hcat $ intersperse vSep row
    hSep = hrule (80 * fromIntegral rowSize) # dashing [5] 0 # lc gray
    vSep = vrule 80 # dashing [5] 0 # lc gray
    cell (n, q) = centerY (qrcDiagram q === txtD) <> square 80 # lw 0 where
        txtD = baselineText n # scale 10 # translate t # clipBy (rect 70 20) <>
               rect 80 20 # lw 0
        t = (-35) & (-5)
        qrcDiagram code = centerX $ vcat $ map (hcat . map qrTile) code where
            qrTile x = square 2 # fc (if x == 0 then white else black) # lw 0

-- Command line options type
data UserOptions = UserOptions {optOutput :: Maybe FilePath,
                                optInput :: Maybe FilePath,
                                optHelp :: Bool} deriving Show

-- The default command line options 
defaultOptions :: UserOptions
defaultOptions = UserOptions {optOutput = Nothing, optInput = Nothing,
                              optHelp = False}

-- Command line options definition
options :: [OptDescr (UserOptions -> UserOptions)]
options = [GetOpt.Option "o" ["output"]
           (ReqArg (\f opts -> opts {optOutput = Just f}) "FILE")
           "output FILE",
           GetOpt.Option "i" ["input"]
           (ReqArg (\f opts -> opts {optInput = Just f}) "FILE")
           "input FILE",
           GetOpt.Option "h" ["help", "usage"]
           (NoArg (\opts -> opts {optHelp = True})) "This help message"]

-- Parse command line options into an options type
getOptions :: IO UserOptions
getOptions = do
    argv <- getArgs
    name <- getProgName
    case getOpt Permute options argv of
       (o,_,[]) -> return $ foldl (flip id) defaultOptions o
       (_,_,er) -> ioError $ userError $ concat er ++ usageInfo header options
           where header = "Usage: " ++ name ++ " [OPTION...]"

-- Get input from standard input or from a file, depending on the options
getInputContents :: UserOptions -> IO String
getInputContents UserOptions {optInput = Nothing} = getContents
getInputContents UserOptions {optInput = Just f} = readFile f

-- Determine the Cairo output type based on the file path's extension
getFileType :: FilePath -> OutputType
getFileType filePath
    | ".png" `isSuffixOf` filePath = PNG
    | ".ps"  `isSuffixOf` filePath = PS
    | ".pdf" `isSuffixOf` filePath = PDF
    | ".svg" `isSuffixOf` filePath = SVG
    | otherwise = error "Unknown file type"

-- The program: parse command line options, read input and render graphics
main :: IO ()
main = do
    opts <- getOptions
    if optHelp opts then do
        name <- getProgName
        let header = "Usage: " ++ name ++ " [OPTION...]" in
            putStr $ usageInfo header options
    else do
        contents <- getInputContents opts
        g <- getStdGen
        (codes, _) <- assign g $ lines contents
        let path = fromMaybe "output.pdf" $ optOutput opts
            outTy = getFileType path in
            fst $ renderDia Cairo (CairoOptions path (Width 800) outTy False) $
                  friendsDiagram 8 codes

