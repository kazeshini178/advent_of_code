module AOC2025.Day9 where

import Data.List qualified as List
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Text qualified as T
import Text.Parsec (char, digit, many, many1, newline, optional)
import Text.Parsec.String (Parser)
import Utils qualified as U

data Coord = Coord Int Int deriving (Show, Eq, Ord)

parseInstruction :: Parser [Coord]
parseInstruction =
  many $ Coord <$> number <* char ',' <*> number <* optional newline
  where
    number = read <$> many1 digit

part1 :: [String] -> Int
part1 i = snd $ List.maximumBy (comparing snd) distinctPairs
  where
    input = U.runParser parseInstruction $ unlines i
    distinctPairs =
        [ ((c1, c2), area)
        | a <- [0 .. length input - 2],
          let c1 = input !! a,
          b <- [a + 1 .. length input - 1],
          let c2 = input !! b,
          let area = calcArea c1 c2
        ]
    calcArea (Coord x1 y1) (Coord x2 y2) = width * height
      where
        width = abs (x1 - x2) + 1
        height = abs (y1 - y2) + 1


-- Optimized point-in-polygon: first check if point lies on polygon edges
-- Build scanline intervals for horizontal and vertical lines
buildHorizontalIntervals :: V.Vector Coord -> Map.Map Int [(Double, Double)]
buildHorizontalIntervals verts =
  let ys = List.nub $ V.toList $ V.map (\(Coord _ y) -> y) verts
      n = V.length verts
      edges = [(verts V.! i, verts V.! ((i+1) `mod` n)) | i <- [0..n-1]]
      intervalsForY y =
        let xs = [ fromIntegral x1' + fromIntegral (y - y1') * fromIntegral (x2' - x1') / fromIntegral (y2' - y1')
                 | (Coord x1' y1', Coord x2' y2') <- edges, y1' /= y2',
                   let ymin = min y1' y2', let ymax = max y1' y2', y >= ymin, y < ymax ]
            sorted = List.sort xs
            paired = pairwise sorted
        in paired
  in Map.fromList [(y, intervalsForY y) | y <- ys]

buildVerticalIntervals :: V.Vector Coord -> Map.Map Int [(Double, Double)]
buildVerticalIntervals verts =
  let xs = List.nub $ V.toList $ V.map (\(Coord x _) -> x) verts
      n = V.length verts
      edges = [(verts V.! i, verts V.! ((i+1) `mod` n)) | i <- [0..n-1]]
      intervalsForX x =
        let ys = [ fromIntegral y1' + fromIntegral (x - x1') * fromIntegral (y2' - y1') / fromIntegral (x2' - x1')
                 | (Coord x1' y1', Coord x2' y2') <- edges, x1' /= x2',
                   let xmin = min x1' x2', let xmax = max x1' x2', x >= xmin, x < xmax ]
            sorted = List.sort ys
            paired = pairwise sorted
        in paired
  in Map.fromList [(x, intervalsForX x) | x <- xs]

pairwise :: [Double] -> [(Double, Double)]
pairwise (a:b:rest) = (a,b) : pairwise rest
pairwise _ = []

-- Test if integer closed interval [a,b] is contained in any of the (double) intervals
containedInAny :: Int -> Int -> [(Double, Double)] -> Bool
containedInAny a b = any (\(l,r) -> fromIntegral a + 1e-9 >= l - 1e-9 && fromIntegral b <= r + 1e-9)

-- Check if two vertices from input form a valid rectangle within the polygon
-- Uses precomputed horizontal/vertical scanline intervals for fast checks
isValidRectangleInPolygon :: Coord -> Coord -> Map.Map Int [(Double, Double)] -> Map.Map Int [(Double, Double)] -> Bool
isValidRectangleInPolygon (Coord x1 y1) (Coord x2 y2) hMap vMap =
  let minX = min x1 x2
      maxX = max x1 x2
      minY = min y1 y2
      maxY = max y1 y2
      -- horizontal edges at minY and maxY must be contained in horizontal intervals
      hIntervalsMin = Map.findWithDefault [] minY hMap
      hIntervalsMax = Map.findWithDefault [] maxY hMap
      topOK = containedInAny minX maxX hIntervalsMin
      bottomOK = containedInAny minX maxX hIntervalsMax
      -- vertical edges at minX and maxX must be contained in vertical intervals
      vIntervalsMin = Map.findWithDefault [] minX vMap
      vIntervalsMax = Map.findWithDefault [] maxX vMap
      leftOK = containedInAny minY maxY vIntervalsMin
      rightOK = containedInAny minY maxY vIntervalsMax
  in topOK && bottomOK && leftOK && rightOK

part2 :: [String] -> Int
part2 i = snd $ List.maximumBy (comparing snd) validPairs
  where
    input = V.fromList . U.runParser parseInstruction $ unlines i
    -- precompute scanline intervals
    hMap = buildHorizontalIntervals input
    vMap = buildVerticalIntervals input

    allPairs =
      [ ((c1, c2), area)
      | a <- [0 .. length input - 2],
        let c1@(Coord x1 y1) = input V.! a,
        b <- [a + 1 .. length input - 1],
        let c2@(Coord x2 y2) = input V.! b,
        let minX = min x1 x2, let maxX = max x1 x2,
        let minY = min y1 y2, let maxY = max y1 y2,
        -- quick horizontal containment filter using precomputed intervals
        let hIntervalsMin = Map.findWithDefault [] minY hMap,
        let hIntervalsMax = Map.findWithDefault [] maxY hMap,
        containedInAny minX maxX hIntervalsMin,
        containedInAny minX maxX hIntervalsMax,
        let area = calcArea c1 c2
      ]

    -- Filter pairs that form valid rectangles within the polygon, in parallel
    validPairs = filter (\((c1, c2), _) -> isValidRectangleInPolygon c1 c2 hMap vMap) allPairs
      -- `using` parListChunk 100 rseq

    calcArea (Coord x1 y1) (Coord x2 y2) = width * height
      where
        width = abs (x1 - x2) + 1
        height = abs (y1 - y2) + 1

run :: T.Text -> U.Result
run a = U.Result result1 result2
  where
    inputLines = lines $ T.unpack a
    result1 = part1 inputLines
    result2 = part2 inputLines