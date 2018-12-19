{-# LANGUAGE LambdaCase #-}

module Day12.Day12 where

import Control.Arrow
import Data.List
import Text.ParserCombinators.ReadP
import Utils

data Pots = Pots Int String
type Rules = [(String, Char)]

grow :: Pots -> Pots
grow (Pots start pots) =
  let prefix = getPrefix pots
      suffix = getPrefix $ reverse pots
  in Pots (start - length prefix) (prefix <> pots <> suffix)
  where getPrefix = elemIndex '#' >>> \case
          Nothing -> ""
          Just x -> replicate (3 - x) '.'

parseInput :: String -> Maybe (Pots, Rules)
parseInput = parse $ do
  state <- string "initial state: " *> parsePots <* skipSpaces
  rules <- sepBy1 parseRule (char '\n') <* skipSpaces <* eof
  return (state, rules)
  where
    parsePlant = satisfy (`elem` ['.', '#'])
    parsePots = grow . Pots 0 <$> many1 parsePlant
    parseRule = (,) <$> many1 parsePlant <*> (string " => " *> parsePlant)

printPots :: Pots -> IO ()
printPots (Pots start pots)= putStrLn $ show start <> ": " <> pots

step :: Rules -> Pots -> Pots
step rules (Pots x pots) = grow $ Pots (x + 2) (go [] pots) where
  go new (l2:l1:c:r1:r2:rest) = let
    first5 = [l2, l1, c, r1, r2]
    next = maybe '.' snd $ find ((== first5) . fst) rules
    in go (next:new) (drop 1 first5 <> rest)
  go new _ = reverse new

sumPots :: Pots -> Int
sumPots (Pots start pots) = sum $ map fst $ filter ((== '#') . snd) $ zip [start ..] pots

puzzle :: IO ()
puzzle =
  parseInput <$> readFile "./src/Day12/input.txt" >>= \case
    Nothing -> fail "No parse"
    Just (pots, rules) -> do
      let steps = iterate (step rules) pots
      expect "part 1: " 2823 (sumPots (steps !! 20))
      expect "part 2: " "" ""
