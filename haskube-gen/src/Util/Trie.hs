{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Trie where

import           Control.Lens
import           Safe               (minimumDef)

import           Data.Foldable      (foldl', foldr')
import           Data.Function      (on)
import           Data.List          (sortBy)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust, fromMaybe)

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

atPrefix
  :: (Ord token, Applicative f)
  => [token] -> (Trie token -> f (Trie token)) -> Trie token -> f (Trie token)
atPrefix []     = id
atPrefix (x:xs) = trieBranches . at x . _Just . atPrefix xs

branchSize :: (Ord a) => a -> Trie a -> Int
branchSize token = maybe 0 (_trieSize) . preview (atPrefix [token])

deleteBranches :: Trie a -> Maybe (Trie a)
deleteBranches trie@Trie {..} = case _trieLeaf of
  0 -> Nothing
  _ -> Just (trie & trieSize .~ _trieLeaf & trieBranches .~ M.empty)

deleteLeaf :: Trie a -> Maybe (Trie a)
deleteLeaf trie@Trie {..} =
  if M.null _trieBranches
    then Nothing
    else Just (trie & trieSize %~ subtract _trieLeaf & trieLeaf .~ 0)

data Split token = SplitLeaf | Split token deriving (Show, Eq)

splitSizes :: (Ord a) => Trie a -> NonEmpty (Split a, Int)
splitSizes trie =
  M.foldlWithKey f (pure (SplitLeaf, _trieLeaf trie)) $ _trieBranches trie
  where f (a0:|as) token subtrie = a0:|(Split token, _trieSize subtrie):as

validSplits :: (Ord a) => Int -> Trie a -> [Split a]
validSplits minSize trie =
  snd $ foldl' f (0, []) increasingSplits
  where increasingSplits = N.sortWith snd $ splitSizes trie
        f (unsplitSize, splits0) (split1, size1) =
          if unsplitSize < minSize then (unsplitSize + size1, splits0)
          else (unsplitSize, split1:splits0)

-- | Assumes we already know the split is valid.
doSplit :: (Ord a) => Split a  -> Trie a -> (Trie a, Trie a)
doSplit SplitLeaf     = fromJust . splitLeaf
doSplit (Split token) = fromJust . splitBranchAt token

splitLeaf :: (Ord a) => Trie a -> Maybe (Trie a, Trie a)
splitLeaf trie@Trie {..} = do
  branches <- deleteLeaf trie
  leaf <- deleteBranches trie
  return (leaf, branches)

splitBranchAt :: (Ord a) => a -> Trie a -> Maybe (Trie a, Trie a)
splitBranchAt token trie@Trie {..} = do
  branch <- _trieBranches ^. at token
  let theRest =
        trie & trieBranches %~ M.delete token & trieSize %~
        subtract (branch ^. trieSize)
  return (branch, theRest)

splitOne :: (Ord a) => Int -> Trie a -> NonEmpty (Trie a)
splitOne minSize trie_ = foldl' f (pure trie_) splits
  where
    splits = validSplits minSize trie_
    f (trie0 :| accum0) split1 =
      let (a1, trie1) = doSplit split1 trie0
      in (trie1 :| a1 : accum0)
