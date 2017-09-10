{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Util.Trie where

import           Control.Lens
import           Safe               (minimumDef)

import           Data.Foldable      (foldl', foldr')
import           Data.Function      (on)
import           Data.List          (sortOn)
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

data Split token
  = SplitLeaf -- ^ split leaf from branches
  | Split token -- ^ split a branch from the rest
  deriving (Show, Eq)

data TrieSplits split
  = Splits [split] -- ^ the trie has multiple sections
  | Drill split -- ^ the trie has only one section
  deriving (Show, Eq)

splitSizes :: (Ord a) => Trie a -> TrieSplits (Split a, Int)
splitSizes trie = case nonzeroSplits of
  [(SplitLeaf, _)] -> Splits []
  [x]              -> Drill x
  xs               -> Splits xs
  where
    nonzeroSplits = N.filter (\x -> snd x > 0) allSplits
    allSplits =
      M.foldlWithKey f (pure (SplitLeaf, _trieLeaf trie)) $ _trieBranches trie
    f (a0 :| as) token subtrie = a0 :| (Split token, _trieSize subtrie) : as

validSplits :: (Ord a) => Int -> Trie a -> TrieSplits (Split a)
validSplits minSize trie =
  case splitSizes trie of
    Drill (split, _) -> Drill split
    Splits splits -> Splits . snd $ foldl' f (0, []) increasingSplits
      where increasingSplits = sortOn snd splits
            f (unsplitSize, splits0) (split1, size1) =
              if unsplitSize < minSize
                then (unsplitSize + size1, splits0)
                else (unsplitSize, split1 : splits0)

-- | Assumes we already know the split is valid.
doSplit :: (Ord a) => Split a  -> Trie a -> ((Maybe a, Trie a), Trie a)
doSplit SplitLeaf = over _1 (Nothing, ) . fromJust . splitLeaf
doSplit (Split token) = over _1 (Just token, ) . fromJust . splitBranchAt token

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

 --g $ foldl' f (trie_, []) splits
splitOne :: (Ord a) => Int -> Trie a -> NonEmpty (Maybe a, Trie a)
splitOne minSize trie_ =
  case validSplits minSize trie_
  of Drill split -> pure . fst $ doSplit split trie_
     Splits splits -> g $ foldl' f (trie_, []) splits
       where
         f (trie0, accum0) split1 =
           let (a1, trie1) = doSplit split1 trie0
           in (trie1, a1 : accum0)
         g (t, prefixedTs) = (Nothing, t) :| prefixedTs
