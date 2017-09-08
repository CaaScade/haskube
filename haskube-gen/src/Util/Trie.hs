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

deleteBranches :: Trie a -> Maybe (Trie a)
deleteBranches trie@Trie {..} = case _trieLeaf of
  0 -> Nothing
  _ -> Just (trie & trieSize .~ _trieLeaf & trieBranches .~ M.empty)

-- | Uses reversed prefixes.
bestSplit
  :: (Ord a)
  => [([a], Trie a)] -- ^ Tries and their (reversed) prefixes
  -> [([a], Trie a)] -- ^ A Trie has been replaced by its subtries
bestSplit [] = []
bestSplit (x:xs) = splitOnce toSplit ++ unsplit
  where
    (unsplit, (_, toSplit)) = foldr' f ([], (minSubtrieSize $ snd x, x)) xs
    f trie (unsplit, (size0, trie0)) =
      let size = minSubtrieSize $ snd trie
      in if size > size0
           then (trie0 : unsplit, (size, trie))
           else (trie : unsplit, (size0, trie0))

-- | Uses reversed prefixes.
splitOnce :: ([a], Trie a) -> [([a], Trie a)]
splitOnce (prefix, trie_@Trie {..}) = case deleteBranches trie_ of
  Nothing       -> splitBranches
  Just leafTrie -> (prefix, leafTrie):splitBranches
  where f (token, trie) = (token:prefix, trie)
        splitBranches = f <$> M.toList _trieBranches

-- TODO: Splitting methods need to return Maybe so we know if they did any work.
-- | Prefixes are in the correct order.
split :: (Ord a) => Int -> Trie a -> [([a], Trie a)]
split targetSplitCount trie = over _1 reverse <$> loop [([], trie)]
  where loop tries = if length tries > targetSplitCount then loop' tries
                     else loop $ bestSplit tries
        loop' tries = let tries' = bestSplit tries
                      in if length tries' > length tries then tries
                         else loop' tries'
