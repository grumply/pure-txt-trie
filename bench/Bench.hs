module Main where

import Trivial

import qualified Data.Map.Strict as Map

import qualified Data.HashMap.Strict as HashMap

import qualified Pure.Data.Txt as Txt
import qualified Pure.Data.Txt.Trie as Trie

import qualified Data.List as List

import Prelude hiding (lookup)

main = run suite

suite = tests
  [
   scope "fromList" fromList,
   scope "toList" toList,
   scope "lookup" lookup,
   scope "insert" insert
  ]

-- * Lookup
lookup = tests
    [ scope "1" lookupFast
    , scope "1'" lookupFast'
    , scope "7" lookupSlow
    , scope "7'" lookupSlow'
    ]

lookupFast = do
  b0 <- whnf "trie" (Trie.lookup "zzzzzzzzzz") trie
  b1 <- whnf "map"   (Map.lookup "zzzzzzzzzz") mapping
  report b0 b1
  complete

lookupSlow = do
  b0 <- whnf "trie" (Trie.lookup "abcdefgz") trie
  b1 <- whnf "map"   (Map.lookup "abcdefgz") mapping
  report b0 b1
  complete

lookupFast' = do
  b0 <- whnf "trie" (Trie.lookup "zzzzzzzzzz") trie
  b1 <- whnf "hashmap"   (HashMap.lookup "zzzzzzzzzz") hashmapping
  report b0 b1
  complete

lookupSlow' = do
  b0 <- whnf "trie" (Trie.lookup "abcdefgz") trie
  b1 <- whnf "hashmap"   (HashMap.lookup "abcdefgz") hashmapping
  report b0 b1
  complete


-- * Insert
insert = tests
    [ scope "shared" insertShared
    , scope "unrelated" insertUnrelated
    , scope "unrelated'" insertUnrelated'
    ]

insertShared = do
  b0 <- whnf "trie" (Trie.insert "abcdefgzzzz" 1) trie
  b1 <- whnf "map"   (Map.insert "abcdefgzzzz" 1) mapping
  report b0 b1
  complete

insertUnrelated = do
  b0 <- whnf "trie" (Trie.insert "xxxxxxxxxxx" 1) trie
  b1 <- whnf "map"   (Map.insert "xxxxxxxxxxx" 1) mapping
  report b0 b1
  complete

insertUnrelated' = do
  b0 <- whnf "trie" (Trie.insert "xxxxxxxxxxx" 1) trie
  b1 <- whnf "hashmap"   (HashMap.insert "xxxxxxxxxxx" 1) hashmapping
  report b0 b1
  complete

-- * FromList

fromList = tests
    [ scope "small" fromListSmall
    , scope "small'" fromListSmall'
    , scope "large" fromListLarge
    , scope "large'" fromListLarge'
    ]

fromListSmall = do
  b0 <- nf "trie" Trie.fromList small
  b1 <- nf "map"   Map.fromList small
  report b0 b1
  complete

fromListLarge = do
  b0 <- nf "trie" Trie.fromList large
  b1 <- nf "map"   Map.fromList large
  report b0 b1
  complete

fromListSmall' = do
  b0 <- nf "trie" Trie.fromList small
  b1 <- nf "hashmap" HashMap.fromList small
  report b0 b1
  complete

fromListLarge' = do
  b0 <- nf "trie" Trie.fromList large
  b1 <- nf "hashmap" HashMap.fromList large
  report b0 b1
  complete



-- * ToList

-- | Performance here is deceptive; if you write something like:
--
-- > do
-- >   let t = _.fromList large
-- >       l = _.toList t
-- >   print (List.length l)
--
-- where _ is {Trie,Map}, you'll see only a slight difference in runtimes,
-- but this `toList` benchmark shows a 20x slowdown relative to Map.
-- I have a feeling real-world uses will not see the slowdown.
toList = tests
    [ scope "small" toListSmall
    , scope "large" toListLarge
    ]

toListSmall = do
  b0 <- nf "trie" Trie.toList smallTrie
  b1 <- nf "map"   Map.toList smallMapping
  report b0 b1
  complete

toListLarge = do
  b0 <- nf "trie" Trie.toList trie
  b1 <- nf "map"   Map.toList mapping
  report b0 b1
  complete

-- * Utilities

{-# NOINLINE small #-}
small = take 100 large

-- The tests for HashMap get a little funky if
-- we add an extra letter to the dataset; not
-- sure what that's about; should probably look
-- into that to see if it is a HashMap problem
-- or a Trivial problem.
{-# NOINLINE large #-}
large = let sharedPrefix = "abcdefg" in
    ("zzzzzzzzzz",1) :
    [ (sharedPrefix `Txt.append` Txt.pack [w,x,y]
      ,0 :: Int
      )
    | w <- ['a'..'z']
    , x <- ['a'..'z']
    , y <- ['a'..'z']
    ]

{-# NOINLINE trie #-}
trie = Trie.fromList large

{-# NOINLINE mapping #-}
mapping = Map.fromList large

{-# NOINLINE hashmapping #-}
hashmapping = HashMap.fromList large

{-# NOINLINE smallTrie #-}
smallTrie = Trie.fromList small

{-# NOINLINE smallMapping #-}
smallMapping = Map.fromList small
