{-# LANGUAGE LambdaCase #-}

module Aoc2018.Day7.Day7 where

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
buildDeps deps = M.unionWith (<>) targets withDeps
  where targets = foldMap (flip M.singleton S.empty . fst) deps
        withDeps = M.fromListWith S.union . map (second S.singleton . swap) $ deps

notBlocked :: Dependencies -> String
notBlocked = M.keys . M.filter S.null

dropTasks :: S.Set Char -> Dependencies -> Dependencies
dropTasks tasks deps = M.map dropDependency $ M.withoutKeys deps tasks
  where dropDependency target = S.difference target tasks

stepTime :: Char -> Int
stepTime = (+61) . subtract (fromEnum 'A') . fromEnum

type Active = (Char, Int)
tickActive :: [Active] -> (S.Set Char, [Active])
tickActive = foldr (extractDone . second (subtract 1)) (S.empty, [])
  where extractDone (c, 0) (done, active) = (S.insert c done, active)
        extractDone task   (done, active) = (done, task:active)

type Workers = Int
timedWorkflow :: Workers -> Dependencies -> (Int, String)
timedWorkflow workers = go 0 "" []
  where go time flow active deps
          | M.null deps && null active = (time - 1, flow)
          | otherwise = let
              (done, active') = tickActive active
              deps' = dropTasks done deps
              freeWorkers = workers - length active'
              next = take freeWorkers $ notBlocked deps'
              deps'' = M.withoutKeys deps' (S.fromList next)
              flow' = flow <> S.toList done
              active'' = active' <> map (id &&& stepTime) next
              in go (time + 1) flow' active'' deps''


puzzle :: IO ()
puzzle =
  parseDeps <$> readLines "./src/Aoc2018/Day7/input.txt" >>= \case
    Nothing -> fail "no parse"
    Just deps -> do
      let
        depsMap = buildDeps deps
        (_, workflow) = timedWorkflow 1 depsMap
      expect "part 1: " "CQSWKZFJONPBEUMXADLYIGVRHT" workflow

      let (elapsed, _) = timedWorkflow 5 depsMap
      expect "part 2: " 914 elapsed
