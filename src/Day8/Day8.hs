{-# LANGUAGE LambdaCase #-}
module Day8.Day8 where

import Text.Read (readMaybe)
import Utils

newtype Node = N ([Node], [Int]) -- ^ (children, metadata)

parseNodes :: [Int] -> Node
parseNodes = fst . go
  where go (nchld : nmeta : rest) = let
          (children, rest') = foldr parseChild ([], rest) (replicate nchld go)
          (meta, rest'') = splitAt nmeta rest'
          in (N (reverse children, meta), rest'')
        go _ = undefined
        parseChild f (children, rest) = let
          (child, rest') = f rest
          in (child:children, rest')

getMeta :: Node -> [Int]
getMeta (N (children, meta)) = meta <> concatMap getMeta children

nodeValue :: Node -> Int
nodeValue (N ([], meta)) = sum meta
nodeValue (N (children, meta))= sum $ map childSum meta
  where childSum childIdx = let
          value = case  drop (childIdx - 1) children of
                [] -> 0
                (child:_) -> nodeValue child
          in value

puzzle :: IO ()
puzzle =
  traverse readMaybe . words <$> readFile "./src/Day8/input.txt" >>= \case
    Nothing -> fail "No parse"
    Just input -> do
      let tree = parseNodes input
      expect "part 1: " 44338 (sum $ getMeta tree)
      expect "part 2: " 37560 (nodeValue tree)
