{-# LANGUAGE LambdaCase #-}

module Aoc2018.Day6.Day6 where

import           Control.Arrow
import           Control.Monad (forM_, when)
import           Data.Function (on)
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import qualified Data.Set as S
import           Text.Read
import           Utils

type Dim = Point
type PointId = Char

pointIds :: String
pointIds = ['a' .. 'z'] ++ ['A' .. 'Z']

parsePoints :: [String] -> Maybe [(Char, Point)]
parsePoints = fmap (zip pointIds) . traverse readPoint
  where readPoint line = readMaybe ("(" <> line <> ")")

distances :: [(PointId, Point)] -> [Point] -> [[(PointId, Int)]]
distances points = map distance
  where distance coord = map (second (manhattanDistance coord)) points

groupPoints :: [(PointId, Int)] -> [[(PointId, Int)]]
groupPoints = L.groupBy ((==) `on` snd) . L.sortBy (compare `on` snd)

type DistanceMap = (Bounds, String)
distanceMap :: [(PointId, Point)] -> DistanceMap
distanceMap points = (bounds, map fillCell $ distances points (getCoords bounds))
  where bounds = getBounds $ map snd points
        fillCell = cellChar . groupPoints
        cellChar ([(x, _)]:_) = x
        cellChar _ = '.'

borderChars :: DistanceMap -> S.Set Char
borderChars (bounds, coords) = S.fromList $ catMaybes isBorder
  where border = map onBorder (getCoords bounds)
        isBorder = zipWith (\b x -> if b then Just x else Nothing) border coords
        ((minX, minY), (maxX, maxY)) = bounds
        onBorder (x, y) = x `elem` [minX, maxX] || y `elem` [minY, maxY]

printWorld :: DistanceMap -> IO ()
printWorld (bounds, cells) = let
  ((min', _), (max', _)) = bounds
  width = 1 + max' - min'
  in forM_ (zip [0 ..] cells) $ \(idx, point) -> do
    putStr [point]
    when (idx `mod` width == width - 1) $ putStr "\n"

puzzle :: IO ()
puzzle =
  parsePoints <$> readLines "./src/Aoc2018/Day6/input.txt" >>= \case
    Nothing -> fail "No parse"
    Just points -> do
      let
        world = distanceMap points
        ignoreChars = S.insert ':' $ borderChars world
        dropInfinite = filter (`S.notMember` ignoreChars)
        lengths = map length $ L.group $ L.sort $ dropInfinite $ snd world
      expect "part 1: " 3223 (maximum lengths)

      let
        (bounds, _) = world
        dists = distances points (getCoords bounds)
        accum = map (sum . map snd) dists
        central = filter (< 10000) accum
      expect "part 2: " 40495 (length central)
