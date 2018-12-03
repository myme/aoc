{-# LANGUAGE TupleSections #-}

module Day3.Day3 where

import           Control.Monad
import           Data.Char (isDigit)
import           Data.List (find)
import           Data.Vector (Vector, (++))
import qualified Data.Vector as V
import           Prelude hiding ((++))
import           Text.ParserCombinators.ReadP

data Claim = Claim { _id :: Int
                   , _left :: Int
                   , _top :: Int
                   , _width :: Int
                   , _height :: Int
                   } deriving Show

type Dim = (Int, Int) -- ^ (w, h)
data Fabric = Fabric Dim (Vector Int)

empty :: Fabric
empty = Fabric (w, h) $ V.replicate (w * h) 0
  where w = 1000; h = 1000

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseClaim :: ReadP Claim
parseClaim = Claim
  <$> (char '#' >> parseInt)
  <*> (string " @ " >> parseInt)
  <*> (char ',' >> parseInt)
  <*> (string ": " >> parseInt)
  <*> (char 'x' >> parseInt <* eof)

parseClaims :: [String] -> [Claim]
parseClaims = map fst . concatMap (readP_to_S parseClaim)

mergeClaim :: Fabric -> Claim -> Fabric
mergeClaim (Fabric (w, h) v) c = Fabric (w, h) v'
  where v' = V.accum (+) v (map (, 1) $ claimIdxs c)
        claimIdxs (Claim _ x y w h) = do
          column <- [x .. x + w - 1]
          row    <- [y .. y + h - 1]
          return $ index (column, row)
        index (x, y) = x + y * w

puzzle :: IO ()
puzzle = do
  claims <- parseClaims . lines <$> readFile "./src/Day3/input.txt"

  let (Fabric _ merged) = foldr (flip mergeClaim) empty claims
      part1 = show (V.length $ V.filter (> 1) merged)
  putStrLn $ "part1: " <> part1

  -- let single = V.map (\x -> if x == 1 then 1 else 0) merged
  -- putStrLn $ "part2: "
