{-# LANGUAGE TemplateHaskell #-}

module Day15.Day15 where

import           Control.Monad.State
import           Data.Either
import qualified Data.Set as S
import           Lens.Micro.Platform
import           Prelude hiding (round)
import           Utils

type Coord = (Int, Int)
type World = S.Set Coord
data GameState = G { _gameUnits :: [Unit]
                   , _gameRound :: Int
                   , _gameWorld :: World
                   }
type Game = State GameState
data UnitType = Elf | Goblin
data Unit = U { _unitType :: UnitType
              , _unitCoord :: Coord
              , _unitHP :: Int
              }

makeLenses ''GameState
makeLenses ''Unit

initGame :: [String] -> GameState
initGame ls = let
  cells = do
    (y, l) <- zip [0 ..] ls
    (x, c) <- zip [0 ..] l
    let coord = (x, y)
    case c of
      '#' -> [Right coord]
      'E' -> [Left $ U Elf coord 200]
      'G' -> [Left $ U Goblin coord 200]
      _ -> []
  in G (lefts cells) 0 (S.fromList (rights cells))

round :: Game ()
round = do
  undefined

game :: Game ()
game = return ()

combat :: [String] -> Int
combat input = let
  G units round _ = execState game (initGame input)
  totalHP = sum (map (view unitHP) units)
  in round * totalHP

puzzle :: IO ()
puzzle = do
  expect "part 1: " 27730 (combat
                           ["#######"
                           ,"#.G...#"
                           ,"#...EG#"
                           ,"#.#.#G#"
                           ,"#..G#E#"
                           ,"#.....#"
                           ,"#######"
                           ])
  expect "part 2: " "" ""
