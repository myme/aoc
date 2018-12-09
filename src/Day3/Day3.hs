module Day3.Day3 where

import           Control.Monad
import qualified Data.IntSet as I
import qualified Data.Ix as Ix
import           Data.List (find)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Prelude hiding ((++))
import           Text.ParserCombinators.ReadP
import           Utils

type Pos = (Int, Int) -- ^ (x, y)
type Dim = (Int, Int) -- ^ (w, h)
data Fabric = Fabric { _dim :: Dim, _vect :: Vector Int }
data Claim = Claim {_id :: Int, _idxs :: [Pos] }

empty :: Fabric
empty = Fabric (w, h) $ V.replicate (w * h) 0
  where w = 1000; h = 1000

index :: Dim -> Pos -> Int
index dim = Ix.index ((0, 0), dim)

parseClaim :: ReadP Claim
parseClaim = do
  id' <- char '#' >> parseInt
  x <- string " @ " >> parseInt
  y <- char ',' >> parseInt
  w <- string ": " >> parseInt
  h <- char 'x' >> parseInt <* eof
  return $ Claim id' (Ix.range ((x, y), (x + w - 1, y + h -1)))

parseClaims :: [String] -> [Claim]
parseClaims = map fst . concatMap (readP_to_S parseClaim)

applyClaims :: Fabric -> [Claim] -> Fabric
applyClaims (Fabric dim vect) claims = Fabric dim (V.modify apply vect)
  where apply v = forM_ claims $
          \(Claim _ idxs) -> forM_ idxs (VM.modify v (+ 1) . index dim)

puzzle :: IO ()
puzzle = do
  claims <- parseClaims . lines <$> readFile "./src/Day3/input.txt"

  let (Fabric dim merged) = applyClaims empty claims
      part1 = V.length $ V.filter (> 1) merged

  expect "part1: " 101469 part1

  let indexed = V.filter ((== 1) . snd) $ V.indexed merged
      indexes = I.fromList $ V.toList $ V.map fst indexed
      pred' (Claim _ idxs) = I.fromList (map (index dim) idxs) `I.isSubsetOf` indexes
      part2 = find pred' claims

  expect "part2: " 1067 $ maybe 0 _id part2
