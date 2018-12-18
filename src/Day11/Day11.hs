{-# LANGUAGE TupleSections #-}

module Day11.Day11 where

import           Control.Monad.Reader
import           Data.Ix
import           Utils

type Coord = (Int, Int) -- ^ (x, y)
type Dim = (Coord, Coord)
type Puzzle a = Reader (Int, Dim) a
type Power = Int
type Size = Int

cellPower :: Coord -> Puzzle Power
cellPower (x, y) = do
  gsn <- asks fst
  let
    rack = x + 10
    pInit = rack * y
    pGrid = pInit + gsn
    pRack = pGrid * rack
    hunds = pRack `div` 100 `mod` 10
  return $ hunds - 5

type SquareSize = Int
squareTotal :: SquareSize -> Coord -> Puzzle Int
squareTotal sz coord = do
  let
    (x, y) = coord
    rs = range (coord, (x + sz - 1, y + sz - 1))
  aggregates <- mapM cellPower rs
  return (sum aggregates)


run :: Puzzle a -> a
run = flip runReader (gsn, dim) where
  gsn = 5791
  dim = ((1, 1), (300, 300))

puzzle :: IO ()
puzzle = do
  let (part1, _) = run $ do
        coords <- range <$> asks snd
        cells <- zip coords <$> mapM (squareTotal 3) coords
        return $ maxBy snd cells
  expect "part 1: " (20, 68) part1

  let ((x, y), size, _) = run $ do
        cells <- forM [1 .. 20] $ \sz -> do
          coords <- range <$> asks snd
          cells <- zip coords <$> mapM (squareTotal sz) coords
          let (c, p) = maxBy snd cells
          return (c, sz, p)
        return $ maxBy (\(_, _, p) -> p) cells

  expect "part 2: " (231, 273, 16) (x, y, size)
