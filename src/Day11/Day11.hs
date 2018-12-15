module Day11.Day11 where

import           Control.Monad.Reader
import           Data.Ix
import qualified Data.Map.Strict as M
import           Lens.Micro.Platform
import           Utils

type Puzzle a = Reader Int a
type Coord = (Int, Int) -- ^ (x, y)
type Cells = M.Map Coord Int

cellPower :: Coord -> Puzzle Int
cellPower (x, y) = do
  gsn <- ask
  let
    rack = x + 10
    pInit = rack * y
    pGrid = pInit + gsn
    pRack = pGrid * rack
    hunds = pRack `div` 100 `mod` 10
  return $ hunds - 5

squareTotal :: Coord -> Puzzle Int
squareTotal (x, y) = do
  let rs = range ((x, y), (x + 2, y + 2))
  aggregates <- mapM cellPower rs
  return $ sum aggregates

puzzle :: IO ()
puzzle = do
  let gsn = 5791
  -- let gsn = 18
  -- let gsn = 42
      dim = ((1, 1), (300, 300))
      cells = range $ over (_2 . both) (subtract 2) dim
      coord = maxBy (flip runReader gsn . squareTotal) cells
  expect "part 1: " (20, 68) coord
  expect "part 2: " "" ""
