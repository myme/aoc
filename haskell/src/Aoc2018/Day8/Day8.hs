{-# LANGUAGE LambdaCase #-}

module Aoc2018.Day8.Day8 where

import Control.Arrow (first)
import Text.Read (readMaybe)
import Utils

newtype Node = N ([Node], [Int]) -- ^ (children, metadata)

parseNodes :: [Int] -> Node
parseNodes = fst . go
  where go (nchld : nmeta : rest) = let
          (children, rest') = foldr parseChild ([], rest) [1 .. nchld]
          (meta, rest'') = splitAt nmeta rest'
          in (N (reverse children, meta), rest'')
        go _ = undefined
        parseChild _ (children, rest) = first (:children) $ go rest

getMeta :: Node -> [Int]
getMeta (N (children, meta)) = meta <> concatMap getMeta children

nodeValue :: Node -> Int
nodeValue (N ([], meta)) = sum meta
nodeValue (N (children, meta))= sum $ map childSum meta
  where childSum childIdx = case drop (childIdx - 1) children of
          [] -> 0
          (child:_) -> nodeValue child

puzzle :: IO ()
puzzle =
  traverse readMaybe . words <$> readFile "./src/Aoc2018/Day8/input.txt" >>= \case
    Nothing -> fail "No parse"
    Just input -> do
      let tree = parseNodes input
      expect "part 1: " 44338 (sum $ getMeta tree)
      expect "part 2: " 37560 (nodeValue tree)
