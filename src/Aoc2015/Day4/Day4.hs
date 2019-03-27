module Aoc2015.Day4.Day4 where

import           Control.Arrow ((&&&))
import           Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import           Data.List

md5 :: BS.ByteString -> String
md5 = show . (hash :: BS.ByteString -> Digest MD5)

input :: BS.ByteString
input = "iwrupvqb"

md5list :: [(Int, String)]
md5list = map (id &&& compute) [1 ..]
  where compute = md5 . (input <>) . BS.pack . show

findMd5 :: Int -> IO Int
findMd5 n = do
  let prefix = replicate n '0'
      result = fst <$> find (isPrefixOf prefix . snd) md5list
  maybe (fail "md5 not found") pure result

part1 :: IO Int
part1 = findMd5 5

part2 :: IO Int
part2 = findMd5 6
