{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Util.Trie where

import           Control.Lens
import           Control.Monad.Writer
import           Safe                 (minimumDef)

import           Data.Foldable        (foldl', foldr')
import           Data.Function        (on)
import           Data.List            (sortOn)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as N
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, fromMaybe, maybeToList)

import           Debug.Trace

data Trie token = Trie
  { _trieSize     :: Int -- ^ Total leaves in this trie.
  , _trieBranches :: Map token (Trie token)
  , _trieLeaf     :: Int -- ^ How many entries end at the root of this trie?
  } deriving (Show, Eq)

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
branchSize token = maybe 0 _trieSize . preview (atPrefix [token])

deleteBranches :: Trie a -> Maybe (Trie a)
deleteBranches trie@Trie {..} = case _trieLeaf of
  0 -> Nothing
  _ -> Just (trie & trieSize .~ _trieLeaf & trieBranches .~ M.empty)

deleteLeaf :: Trie a -> Maybe (Trie a)
deleteLeaf trie@Trie {..} =
  if M.null _trieBranches
    then Nothing
    else Just (trie & trieSize %~ subtract _trieLeaf & trieLeaf .~ 0)

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

{- |
Walk through the tree (trie) depth first and build up a list of adequate
  subtries along the way.
-}

-- | Prefixes are in reverse order.
collapse
  :: (Ord a)
  => ([b] -> ([a], Trie a) -> b)
  -> ([a], Trie a)
  -> b
collapse f x@(rprefix, trie@Trie {..}) =
  f branchResults x
  where
    branches = over _1 (: rprefix) <$> M.toList _trieBranches
    branchResults = collapse f <$> branches

data Doot a = Doot { _dootClaimed   :: [([a], Int)]
                   , _dootUnclaimed :: [([a], Int)] -- ^ Just in case we want to do something clever in the future.
                   } deriving (Show, Eq)

dootUnclaimedTotal :: Doot a -> Int
dootUnclaimedTotal Doot {..} = sum . fmap snd $ _dootUnclaimed

-- TODO: use the rprefix in _dootUnclaimed to keep tiny branches from bubbling all the way to the root.
foop :: (Ord a) => Int -> [Doot a] -> ([a], Trie a) -> Doot a
foop minSize doots (rprefix, trie)
  | totalUnclaimed < minSize =
    Doot
    { _dootClaimed = allClaimed
    , _dootUnclaimed = (rprefix, thisUnclaimedCount) : childUnclaimed
    }
  | otherwise =
    Doot
    {_dootClaimed = (rprefix, totalUnclaimed) : allClaimed, _dootUnclaimed = []}
  where
    thisUnclaimedCount = _trieLeaf trie
    childUnclaimed = _dootUnclaimed =<< doots
    totalUnclaimed = thisUnclaimedCount + sum (snd <$> childUnclaimed)
    allClaimed = concat $ _dootClaimed <$> doots

poot :: [a] -> Doot a -> [([a], Int)]
poot rprefix doot@Doot {..}
  | unclaimedTotal > 0 = (rprefix, unclaimedTotal) : _dootClaimed
  | otherwise = _dootClaimed
  where unclaimedTotal = dootUnclaimedTotal doot

split :: (Ord a) => Int -> Trie a -> [([a], Int)]
split minSize trie = poot [] $ collapse (foop minSize) ([], trie)

