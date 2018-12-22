{-# LANGUAGE TemplateHaskell #-}

module Day14.Day14 where

import Lens.Micro.Platform
import Control.Monad.State
import Utils

data Scoreboard = S { _scores :: [Int]
                    , _elf1 :: Int -- ^ Index
                    , _elf2 :: Int -- ^ Index
                    }
type Chocolatey a = State Scoreboard a

makeLenses ''Scoreboard

initialState :: Scoreboard
initialState = S [3, 7] 0 1

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom count from = take count . drop from

step :: Chocolatey String
step = do
  recipe1 <- (!!) <$> use scores <*> use elf1
  recipe2 <- (!!) <$> use scores <*> use elf2
  let res = show (recipe1 + recipe2)
      new = read . (:"") <$> res
  ss <- scores <%= (<> new)
  elf1 %= \i -> (i + recipe1 + 1) `mod` length ss
  elf2 %= \i -> (i + recipe2 + 1) `mod` length ss
  return res

scoreboard :: String
scoreboard = "37" <> evalState step' initialState
  where step' = concat <$> sequence (repeat step)

puzzle :: IO ()
puzzle = do
  expect "part 1: " "5158916779" (takeFrom 10 9 scoreboard)
  expect "part 1: " "0124515891" (takeFrom 10 5 scoreboard)
  expect "part 1: " "9251071085" (takeFrom 10 18 scoreboard)
  expect "part 1: " "5941429882" (takeFrom 10 2018 scoreboard)
  expect "part 1: " "0000000000" (takeFrom 10 540391 scoreboard)
