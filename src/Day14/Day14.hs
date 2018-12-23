{-# LANGUAGE TemplateHaskell #-}

module Day14.Day14 where

import           Control.Monad.State
import qualified Data.Sequence as S
import           Lens.Micro.Platform
import           Utils

data Scoreboard = S { _scores :: S.Seq Int
                    , _elf1 :: Int -- ^ Index
                    , _elf2 :: Int -- ^ Index
                    }
type Chocolatey a = State Scoreboard a

makeLenses ''Scoreboard

initialState :: Scoreboard
initialState = S (S.fromList [3, 7]) 0 1

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom count from = take count . drop from

step :: Chocolatey String
step = do
  recipe1 <- S.index <$> use scores <*> use elf1
  recipe2 <- S.index <$> use scores <*> use elf2
  let res = show (recipe1 + recipe2)
      new = S.fromList $ fmap (read . (:"")) res
  ss <- scores <%= (<> new)
  elf1 %= \i -> (i + recipe1 + 1) `mod` S.length ss
  elf2 %= \i -> (i + recipe2 + 1) `mod` S.length ss
  return res

scoreboard :: String
scoreboard = "37" <> evalState step' initialState
  where step' = concat <$> sequence (repeat step)

sublistIndex :: Eq a => [a] -> [a] -> Int
sublistIndex _ [] = -1
sublistIndex as xxs@(x:xs)
  | all (uncurry (==)) $ zip as xxs = 0
  | otherwise = 1 + sublistIndex as xs

puzzle :: IO ()
puzzle = do
  let input = 540391
  expect "part 1: " "1474315445" (takeFrom 10 input scoreboard)
  expect "part 2: " 20278122 (sublistIndex (show input) scoreboard)
