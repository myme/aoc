{-# LANGUAGE TupleSections #-}

module Day3.Day3 where

import           Control.Monad
import           Data.Char (isDigit)
import qualified Data.IntSet as I
import           Data.Ix (index, range)
import           Data.List (find)
import           Data.Vector (Vector, (++))
import qualified Data.Vector as V
import           Prelude hiding ((++))
import           Text.ParserCombinators.ReadP

type Pos = (Int, Int) -- ^ (x, y)
type Dim = (Int, Int) -- ^ (w, h)
data Fabric = Fabric { _dim :: Dim, _vect :: Vector Int }
data Claim = Claim {_id :: Int, _idxs :: [Pos] }

empty :: Fabric
empty = Fabric (w, h) $ V.replicate (w * h) 0
  where w = 1000; h = 1000

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseClaim :: ReadP Claim
parseClaim = do
  id <- char '#' >> parseInt
  x <- string " @ " >> parseInt
  y <- char ',' >> parseInt
  w <- string ": " >> parseInt
  h <- char 'x' >> parseInt <* eof
  return $ Claim id (range ((x, y), (x + w - 1, y + h -1)))

parseClaims :: [String] -> [Claim]
parseClaims = map fst . concatMap (readP_to_S parseClaim)

mergeClaim :: Fabric -> Claim -> Fabric
mergeClaim (Fabric d v) (Claim _ idxs) = Fabric d v'
  where v' = V.accum (+) v (map ((, 1) . index ((0, 0), d)) idxs)

puzzle :: IO ()
puzzle = do
  claims <- parseClaims . lines <$> readFile "./src/Day3/input.txt"

  let (Fabric dim merged) = foldr (flip mergeClaim) empty claims
      part1 = show (V.length $ V.filter (> 1) merged)

  putStrLn $ "part1: " <> part1

  let indexed = V.filter ((== 1) . snd) $ V.indexed merged
      indexes = I.fromList $ V.toList $ V.map fst indexed
      pred (Claim _ idxs) = I.fromList (map (index ((0, 0), dim)) idxs) `I.isSubsetOf` indexes
      part2 = find pred claims

  putStrLn $ "part2: " <> maybe "sorry, no matches" (show . _id) part2
