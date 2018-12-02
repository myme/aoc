module Day2.Day2 where

import qualified Data.IntSet as ISet
import           Data.List (group, sort)
import           Data.Maybe (catMaybes)

doublesAndTriples :: String -> (Int, Int)
doublesAndTriples = (\x -> (has x 2, has x 3))
  . ISet.fromList -- create a set of unique lengths
  . map length    -- compute length of sublists
  . group         -- split into sub-lists of equal characters
  . sort          -- sort characters together
  where has set x = fromEnum $ x `ISet.member` set

input :: IO [String]
input = lines <$> readFile "./src/Day2/input.txt"

part1 :: IO Int
part1 = do
  (doubles, triples) <- unzip . map doublesAndTriples <$> input
  return $ sum doubles * sum triples

diffStrings :: String -> String -> [Maybe Char]
diffStrings = zipWith $ \x y -> if x == y then Just x else Nothing

offByOne :: [Maybe Char] -> Bool
offByOne = (== 1) . length . filter (== Nothing)

part2 :: IO [String]
part2 = do
  ls <- input
  let diffs = [diffStrings s1 s2 | s1 <- ls, s2 <- ls]
  return $ take 1 $ map catMaybes $ filter offByOne diffs

puzzle :: IO ()
puzzle = do
  putStr "Answer to part1: "
  part1 >>= print
  putStr "Answer to part 2: "
  part2 >>= print
