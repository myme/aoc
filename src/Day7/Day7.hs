{-# LANGUAGE LambdaCase #-}
module Day7.Day7 where

import           Control.Arrow
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Tuple (swap)
import           Text.ParserCombinators.ReadP
import           Utils

type Dep = (Char, Char) -- ^ (x, y) => x before y

parseDep :: ReadP Dep
parseDep = (,)
  <$> (string "Step " *> get)
  <*> (string " must be finished before step " *> get <* string " can begin." <* eof)

parseDeps :: [String] -> Maybe [Dep]
parseDeps = traverse (fmap fst . listToMaybe . readP_to_S parseDep)

type Dependencies = M.Map Char (S.Set Char) -- ^ Map from target to dependencies
buildDeps :: [Dep] -> M.Map Char (S.Set Char)
buildDeps deps = M.unionWith (<>) initial withDeps
  where initial = foldMap (flip M.singleton S.empty . fst) deps
        withDeps = M.fromListWith S.union . map (second S.singleton . swap) $ deps

generateWorkflow :: Dependencies -> String
generateWorkflow = reverse . go ""
  where notBlocked = M.keys . M.filter S.null
        go flow deps
          | M.null deps = flow
          | otherwise = let
              next = head $ notBlocked deps
              dropNext = S.delete next
              deps' = M.map dropNext $ M.delete next deps
              in go (next:flow) deps'

puzzle :: IO ()
puzzle =
  parseDeps <$> readLines "./src/Day7/input.txt" >>= \case
    Nothing -> fail "no parse"
    Just deps -> do
      let
        workflow = generateWorkflow $ buildDeps deps
      expect "part 1: " "CQSWKZFJONPBEUMXADLYIGVRHT" workflow
      expect "part 2: " "not implemented!" ""
