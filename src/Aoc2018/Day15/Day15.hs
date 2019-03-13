{-# LANGUAGE TemplateHaskell #-}

module Aoc2018.Day15.Day15 where

import           Control.Monad.State
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Tuple
import           Debug.Trace
import           Lens.Micro.Platform
import           Prelude hiding (round)
import           Utils

type Coord = (Int, Int)
type Walls = Set.Set Coord
data CombatState = C { _csUnits :: [Unit]
                     , _csRound :: Int
                     , _csWalls :: Walls
                     }
type Combat = State CombatState
data UnitType = Elf | Goblin deriving Eq
data Unit = U { _unitType :: UnitType
              , _unitCoord :: Coord
              , _unitHP :: Int
              } deriving Eq

instance Show Unit where
  show (U t c h) = type' t <> ":" <> show c <> " => " <> show h
    where type' Elf = "E"
          type' Goblin = "G"

makeLenses ''CombatState
makeLenses ''Unit

initCombat :: [String] -> CombatState
initCombat ls = let
  cells = do
    (y, l) <- zip [0 ..] ls
    (x, c) <- zip [0 ..] l
    let coord = (x, y)
    case c of
      '#' -> [Right coord]
      'E' -> [Left $ U Elf coord 200]
      'G' -> [Left $ U Goblin coord 200]
      _ -> []
  in C (lefts cells) 0 (Set.fromList (rights cells))

isVacant :: Coord -> Combat Bool
isVacant coord = do
  isWall <- Set.member coord <$> use csWalls
  isOccupied <- any ((== coord) . view unitCoord) <$> use csUnits
  return $ not (isWall || isOccupied)

inRangeOf :: Coord -> [Coord]
inRangeOf (x, y) = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]

findPath :: Coord -> Coord -> Combat (Maybe (Coord, Int))
findPath from to' = go 1 Set.empty [to'] where
  go steps dists coords = do
    let dists' = Set.union dists (Set.fromList coords)
        inRange = filter (`Set.notMember` dists') . concatMap inRangeOf $ coords
    -- traceShowM (from, to', inRange)
    case inRange of
      [] -> return Nothing
      xs | any ((== 1) . manhattanDistance from) xs -> return (Just (to', steps))
         | otherwise -> go (steps + 1) dists' inRange

findNearestMove :: [(Coord, Int)] -> Maybe Coord
findNearestMove coords = coords
  & sortBy (compare `on` snd)
  & groupBy ((==) `on` snd)
  & fmap (map fst)
  & listToMaybe
  >>= (listToMaybe . sortBy (compare `on` swap))

stepRound :: Combat ()
stepRound = do
  units <- use csUnits
  forM_ units $ \unit@(U t c _) -> do
    targets <- filter ((/= t) . view unitType) <$> use csUnits
    inRange <- concat <$> traverse (filterM isVacant . inRangeOf . view unitCoord) targets
    reachable <- catMaybes <$> traverse (findPath c) inRange
    let nearest = findNearestMove reachable
    traceShowM (unit, targets, nearest)
    return ()

game :: Combat ()
game = stepRound

combat :: [String] -> Int
combat input = let
  C units round _ = execState game (initCombat input)
  totalHP = sum (fmap (view unitHP) units)
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
