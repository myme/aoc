{-# LANGUAGE LambdaCase #-}
module Day8.Day8 where

import Text.Read (readMaybe)
import Utils

getMeta :: [Int] -> [Int]
getMeta = fst . go 1 []
  where go _ meta [] = (meta, [])
        go 0 meta rest = (meta, rest)
        go n meta (nchild : nmeta : rest) = let
          (childMeta, rest') = go nchild [] rest
          (ownMeta, rest'') = splitAt nmeta rest'
          in go (n - 1) (meta <> ownMeta <> childMeta) rest''
        go _ _ _ = undefined


puzzle :: IO ()
puzzle =
  traverse readMaybe . words <$> readFile "./src/Day8/input.txt" >>= \case

    Nothing -> fail "No parse"
    Just input -> do
      let part1 = sum $ getMeta input
      expect "part 1: " 138 part1
      expect "part 2: " "not implemented!" ""
