{-# LANGUAGE LambdaCase #-}

module Day10.Day10 where

import           Control.Monad (when)
import           Data.Ix (rangeSize)
import qualified Data.Set as S
import           Text.ParserCombinators.ReadP
import           Utils hiding (Point)

type Position = (Int, Int)
type Velocity = (Int, Int)
data Point = P
  { _point :: Position
  , _velocity :: Velocity
  } deriving Show

parsePoint :: String -> Maybe Point
parsePoint = parse $ do
  position <- string "position=" *> parseTuple <* skipSpaces
  velocity <- string "velocity=" *> parseTuple <* eof
  return $ P position velocity
  where
    parseTuple = between (char '<') (char '>') $ do
      a <- skipSpaces *> parseInt <* char ','
      b <- skipSpaces *> parseInt
      return (a, b)

step :: [Point] -> [Point]
step = map translate
  where translate (P p v) = let
          p' = (fst p + fst v, snd p + snd v)
          in P p' v

renderPositions :: [Position] -> String
renderPositions ps = unlines $
  let
    bounds = getBounds ps
    ((minX, minY), (maxX, maxY)) = bounds
    points = S.fromList ps
  in flip map [minY .. maxY] $ \y -> do
    x <- [minX .. maxX]
    return $ if (x, y) `S.member` points then '█' else '.'

stepUntilClosest :: [Point] -> (Int, [Point])
stepUntilClosest = go . zip [0 ..] . iterate step where
  size = rangeSize . getBounds . map _point
  go [] = undefined
  go [_] = undefined
  go ((s,p):(s',p'):ps) | size p < size p' = (s, p)
                        | otherwise = go ((s',p'):ps)

puzzle :: IO ()
puzzle =
  traverse parsePoint <$> readLines "./src/Day10/input.txt" >>= \case
    Nothing -> fail "No parse"
    Just points -> do
      let
        (seconds, ps) = stepUntilClosest points
        message = renderPositions (map _point ps)
        expected_part1 = unlines
          [ "██████..█████.....██....█████...█....█..█....█.....███...████."
          , ".....█..█....█...█..█...█....█..█....█..█....█......█...█....█"
          , ".....█..█....█..█....█..█....█...█..█....█..█.......█...█....."
          , "....█...█....█..█....█..█....█...█..█....█..█.......█...█....."
          , "...█....█████...█....█..█████.....██......██........█...█....."
          , "..█.....█..█....██████..█....█....██......██........█...█....."
          , ".█......█...█...█....█..█....█...█..█....█..█.......█...█....."
          , "█.......█...█...█....█..█....█...█..█....█..█...█...█...█....."
          , "█.......█....█..█....█..█....█..█....█..█....█..█...█...█....█"
          , "██████..█....█..█....█..█████...█....█..█....█...███.....████."
          ]

      putStrLn message
      when (expected_part1 /= message) $ fail "Invalid part1"

      expect "part 2: " 10710 seconds
