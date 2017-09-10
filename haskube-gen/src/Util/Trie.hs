{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Trie where

import           Control.Lens
import           Safe          (minimumDef)

import           Data.Foldable (foldl', foldr')
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)

import           Debug.Trace

data Trie token = Trie
  { _trieSize     :: Int -- ^ Total leaves in this trie.
  , _trieBranches :: Map token (Trie token)
  , _trieLeaf     :: Int -- ^ How many entries end at the root of this trie?
  } deriving (Show)

makeLenses ''Trie

empty :: Trie a
empty = Trie {_trieSize = 0, _trieBranches = M.empty, _trieLeaf = 0}

simpleTestTrie :: Trie Char
simpleTestTrie = insert "aaa" . insert "aab" . insert "ab" $ empty

insert :: (Ord a) => [a] -> Trie a -> Trie a
insert [] = over trieSize (+ 1) . over trieLeaf (+ 1)
insert (x:xs) = over trieSize (+1) . over (trieBranches . at x) f
  where f = return . insert xs . fromMaybe empty

fromList :: (Ord a) => [[a]] -> Trie a
fromList = foldl' (flip insert) empty

-- | Subtrees include the leaf at root.
minSubtrieSize :: Trie a -> Int
minSubtrieSize trie =
  minimumDef (_trieLeaf trie) . fmap _trieSize . M.elems . _trieBranches $ trie

-- | Including the "leaf" branch.
splitCount :: Trie a -> Int
splitCount Trie {..} =
  if _trieLeaf > 0
    then branchCount + 1
    else branchCount
  where
    branchCount = M.size _trieBranches

data SplitScore
  = ScoreMin
  | Score Int
  | ScoreMax
  deriving (Show, Eq)

instance Ord SplitScore where
  compare ScoreMin ScoreMin   = EQ
  compare ScoreMin _          = LT
  compare ScoreMax ScoreMax   = EQ
  compare ScoreMax _          = GT
  compare (Score x) (Score y) = compare x y
  compare (Score _) ScoreMin  = GT
  compare (Score _) ScoreMax  = LT

mkSplitScore :: Int -> SplitScore
mkSplitScore 0 = ScoreMin
mkSplitScore x = Score x

-- | Split quality
splitScore :: Trie a -> SplitScore
splitScore trie@Trie {..} =
  if _trieLeaf > 0
    then if M.null _trieBranches
         then ScoreMin -- Dead end.
         else mkSplitScore $ minSubtrieSize trie -- Split the "largest" branchings.
    else if M.size _trieBranches == 1
         then ScoreMax -- Always chase the long threads.
         else mkSplitScore $ minSubtrieSize trie -- Split the "largest" branchings.

deleteBranches :: Trie a -> Maybe (Trie a)
deleteBranches trie@Trie {..} = case _trieLeaf of
  0 -> Nothing
  _ -> Just (trie & trieSize .~ _trieLeaf & trieBranches .~ M.empty)

-- | Uses reversed prefixes.
bestSplit
  :: (Ord a)
  => [([a], Trie a)] -- ^ Tries and their (reversed) prefixes
  -> (SplitScore, [([a], Trie a)]) -- ^ A Trie has been replaced by its subtries & The split score of the subtrie that was split.
bestSplit [] = (ScoreMin, [])
bestSplit tries@(x:xs) =
  if score > ScoreMin
    then (score, splitOnce toSplit ++ unsplit)
    else (score, tries)
  where
    (unsplit, (score, toSplit)) = foldr' f ([], (splitScore $ snd x, x)) xs
    f trie1 (unsplit, (score0, trie0)) =
      let score1 = splitScore $ snd trie1
      in if score1 > score0
           then (trie0 : unsplit, (score1, trie1))
           else (trie1 : unsplit, (score0, trie0))

-- | Uses reversed prefixes.
splitOnce :: ([a], Trie a) -> [([a], Trie a)]
splitOnce (prefix, trie_@Trie {..}) = case deleteBranches trie_ of
  Nothing       -> splitBranches
  Just leafTrie -> (prefix, leafTrie):splitBranches
  where f (token, trie) = (token:prefix, trie)
        splitBranches = f <$> M.toList _trieBranches

-- TODO: Splitting methods need to return Maybe so we know if they did any work.
-- | Prefixes are in the correct order.
split :: (Ord a, Show a) => Int -> Trie a -> [([a], Trie a)]
split targetSplitCount trie = over _1 reverse <$> loop (ScoreMax, [([], trie)])
  where loop (ScoreMax, tries0) = loop . bestSplit $ tries0
        loop (ScoreMin, tries0) = tries0
        loop (_, tries0)
          | length tries0 > targetSplitCount = tries0
          | otherwise = loop . bestSplit $ tries0

atPrefix
  :: (Ord token, Applicative f)
  => [token] -> (Trie token -> f (Trie token)) -> Trie token -> f (Trie token)
atPrefix []     = id
atPrefix (x:xs) = trieBranches . at x . _Just . atPrefix xs
