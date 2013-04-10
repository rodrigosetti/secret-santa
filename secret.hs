{-# LANGUAGE NoMonomorphismRestriction #-}

-- The Secret Santa assigner
-- Reads a list of friends from the standard input and renders to a specified
-- output file (see --help) all assignments of gift givers and their
-- corresponding receivers encoded in a QR-Code.

import Data.List (intersperse)
import Data.Map (Map, singleton, elems, (!), insert)
import Data.QRCode
import Data.Word (Word8)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
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
friendsMatch :: RandomGen g => g -> [String] -> IO ([(String, QRCodeMatrix)], g)
friendsMatch g friends@(_:_:_) = do
    qrCodes <- mapM qrEncode $ tail shuffled ++ shuffled
    return $ shuffle g' $ zip shuffled $ qrCodes where
        (shuffled, g') = shuffle g friends
        qrEncode s = encodeString s Nothing QR_ECLEVEL_L QR_MODE_EIGHT True >>=
                     return . toMatrix
friendsMatch _ _ = error "Please provide at least two names"

-- Partition a list in intervals, returning a grid
toGrid :: Int -> [a] -> [[a]]
toGrid _ [] = [[]]
toGrid r l = a:(toGrid r b) where
    (a, b) = splitAt r l

-- Renders a list of tuples of giver-name and qr-encoded-receiver-name into a
-- diagram layout as a grid
friendsDiagram :: Int -> [(String, QRCodeMatrix)] -> Diagram Cairo R2
friendsDiagram rowSize l = vcat $ intersperse hSep $
                           map formatRow $ toGrid rowSize $ map cell l where
    formatRow row = centerX $ hcat $ intersperse vSep row
    hSep = (hrule $ 80 * (fromIntegral rowSize)) # dashing [5] 0 # lc gray
    vSep = vrule 80 # dashing [5] 0 # lc gray
    cell (n, q) = centerY (qrcDiagram q === txtD) <> square 80 # lw 0 where
        txtD = baselineText n # scale 10 # translate t # clipBy (rect 70 20) <>
               rect 80 20 # lw 0
        t = (-35) & (-5)
        qrcDiagram code = centerX $ vcat $ map (hcat . (map qrTile)) code where
            qrTile x = square 2 # fc (if x == 0 then white else black) # lw 0

-- Get the pseudo-random number generator; reads the standard input as a list
-- of friends names; randomly assign gift giver and receiver (qr-code encoded);
-- rendering as a grid layout
main :: IO ()
main = do
    g <- getStdGen
    contents <- getContents
    (codes, _) <- friendsMatch g $ lines contents
    defaultMain $ friendsDiagram 8 codes

