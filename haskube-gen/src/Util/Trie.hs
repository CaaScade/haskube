{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Trie where

import           Control.Lens

import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Maybe   (fromMaybe)

data Trie token = Trie
  { _trieSize     :: Int -- ^ Total leaves in this trie.
  , _trieBranches :: Map token (Trie token)
  , _trieLeaf     :: Int -- ^ How many entries end at the root of this trie?
  } deriving (Show)

makeLenses ''Trie

empty :: Trie a
empty = Trie {_trieSize = 0, _trieBranches = M.empty, _trieLeaf = 0}

insert :: (Ord a) => [a] -> Trie a -> Trie a
insert [] = over trieSize (+ 1) . over trieLeaf (+ 1)
insert (x:xs) = over trieSize (+1) . over (trieBranches . at x) f
  where f = return . insert xs . fromMaybe empty
