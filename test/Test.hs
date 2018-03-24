module Main where

import Trivial hiding (some)

import qualified Data.List as List

import Pure.Data.Txt (Txt)
import qualified Pure.Data.Txt as Txt

import Pure.Data.Txt.Trie (TxtTrie,TxtSet)
import qualified Pure.Data.Txt.Trie as Trie

import Prelude hiding (toList,fromList,lookup)

main = run suite

suite = tests
  [
   scope "size" size,
   scope "fromList" fromList,
   scope "toList" toList,
   scope "insert" insert,
   scope "union" union,
   scope "delete" delete,
   scope "difference" difference,
   scope "deletePrefix" deletePrefix,
   scope "structuralEquality" structuralEquality,
   scope "lookup" lookup
  ]

-- * Size
size = tests
    [ scope "empty" sizeEmpty
    , scope "1" sizeOne
    , scope "some" sizeSome
    ]

sizeEmpty =
    let
        test = Trie.size none == 0

    in
        expect test

sizeOne =
    let
        test = Trie.size one == 1

    in
        expect test

sizeSome =
    let
        test = Trie.size some == 8

    in
        expect test

-- * FromList
fromList = tests
    [ scope "empty" fromListEmpty
    , scope "1" fromListOne
    , scope "some" fromListSome
    ]

fromListEmpty =
    let
        setup :: TxtSet
        setup = Trie.fromList []

        test = Trie.size setup == 0

    in
        expect test

fromListOne =
    let
        set :: TxtSet
        set = Trie.fromList [(key,())]

        test = Trie.size set == 1

    in
        expect test

fromListSome =
    expect (Trie.size some == 8)

-- * ToList
toList = tests
    [ scope "empty" toListEmpty
    , scope "1" toListOne
    , scope "some" toListSome
    ]

toListEmpty =
    let
        list = Trie.toList none

        test = List.length list == 0

    in
        expect test

toListOne =
    let
        list = Trie.toList one

        test = List.length list == 1

    in
        expect test

toListSome =
    let
        list = Trie.toList some

        test = List.length list == 8

    in
        expect test

-- * Insert
insert = tests
    [ scope "empty" insertEmpty
    , scope "non-empty" insertNonEmpty
    ]

insertEmpty =
    let
        setup = Trie.insert key () none

        test = Trie.lookup key setup

    in
        expectJust test

insertNonEmpty =
    let
        setup = Trie.insert key () some

        test = Trie.lookup key setup

    in
        expectJust test

-- * Union
union = tests
    [ scope "none-none" unionNoneNone
    , scope "none-one" unionNoneOne
    , scope "none-some" unionNoneSome
    , scope "one-none" unionOneNone
    , scope "one-one" unionOneOne
    , scope "one-some" unionOneSome
    , scope "some-none" unionSomeNone
    , scope "some-one" unionSomeOne
    , scope "some-some" unionSomeSome
    ]

unionNoneNone =
    let
        setup = Trie.union none none

        test = Trie.size setup == 0
    in
        expect test

unionNoneOne =
    let
        setup = Trie.union none one

        test = Trie.size setup == 1

    in
        expect test

unionNoneSome =
    let
        setup = Trie.union none some

        test = Trie.size setup == 8

    in
        expect test

unionOneNone =
    let
        setup = Trie.union one none

        test = Trie.size setup == 1

    in
        expect test

unionOneOne =
    let
        setup = Trie.union one one

        test = Trie.size setup == 1

    in
        expect test

unionOneSome =
    let
        setup = Trie.union one some

        test = Trie.size setup == 9

    in
        expect test

unionSomeNone =
    let
        setup = Trie.union some none

        test = Trie.size setup == 8

    in
        expect test

unionSomeOne =
    let
        setup = Trie.union some one

        test = Trie.size setup == 9

    in
        expect test

unionSomeSome =
    let
        setup = Trie.union some some

        test = Trie.size setup == 8

    in
        expect test

-- * Delete
delete = tests
    [ scope "none" deleteNone
    , scope "one" deleteOne
    ]

deleteNone =
    let
        setup = Trie.delete key none

        test = Trie.size setup == 0

    in
        expect test

deleteOne =
    let
        setup = Trie.delete mempty one

        test = Trie.size setup == 0

    in
        expect test

-- * difference
difference = tests
    [ scope "none-none" differenceNoneNone
    , scope "none-one" differenceNoneOne
    , scope "none-some" differenceNoneSome
    , scope "one-none" differenceOneNone
    , scope "one-one" differenceOneOne
    , scope "one-some" differenceOneSome
    , scope "some-none" differenceSomeNone
    , scope "some-one" differenceSomeOne
    , scope "some-some" differenceSomeSome
    ]

differenceNoneNone =
    let
        setup = Trie.difference none none

        test = Trie.size setup == 0

    in
        expect test

differenceNoneOne =
    let
        setup = Trie.difference none one

        test = Trie.size setup == 0

    in
        expect test

differenceNoneSome =
    let
        setup = Trie.difference none some

        test = Trie.size setup == 0

    in
        expect test

differenceOneNone =
    let
        setup = Trie.difference one none

        test = Trie.size setup == 1

    in
        expect test

differenceOneOne =
    let
        setup = Trie.difference one one

        test = Trie.size setup == 0

    in
        expect test

differenceOneSome =
    let
        setup = Trie.difference one some

        test = Trie.size setup == 1

    in
        expect test

differenceSomeNone =
    let
        setup = Trie.difference some none

        test = Trie.size setup == 8

    in
        expect test

differenceSomeOne =
    let
        setup = Trie.difference some one

        test = Trie.size setup == 8

    in
        expect test

differenceSomeSome =
    let
        setup = Trie.difference some some

        test = Trie.size setup == 0

    in
        expect test

-- * DeletePrefix
deletePrefix = tests
    [ scope "none" deletePrefixNone
    , scope "one" deletePrefixOne
    , scope "some" deletePrefixSome
    ]

deletePrefixNone =
    let
        setup = Trie.deletePrefix "z" some

        test = Trie.size setup == Trie.size some

    in
        expect test

deletePrefixOne =
    let
        setup = Trie.deletePrefix "hobg" some

        test = Trie.size setup == Trie.size some - 1

    in
        expect test

deletePrefixSome =
    let
        setup = Trie.deletePrefix "a" some

        test = Trie.size setup == Trie.size some - 4

    in
        expect test

-- * Lookup
lookup = tests
    [ scope "empty" lookupEmpty
    , scope "non-empty" lookupNonEmpty
    ]

lookupEmpty =
    let
        test = Trie.lookup "" some

    in
        expectNothing test

lookupNonEmpty =
    let
        test = Trie.lookup "a" some

    in
        expectJust test

-- * Structural Equality
structuralEquality =
    let
        a = Trie.difference some $ Trie.fromList (List.take 3 mapping)

        b = Trie.fromList (List.drop 3 mapping)

        test = a /= b && Trie.toList a == Trie.toList b

    in
        expect test

-- * Utilities
none :: TxtSet
none = Trie.empty

one :: TxtSet
one = Trie.fromList [("",())]

some :: TxtSet
some = Trie.fromList mapping

key :: Txt
key = "hobgoblin"

keys :: [Txt]
keys = [ "a", "ab", "abc", "abcd", "h", "ho", "hob", "hobg" ]

mapping :: [(Txt,())]
mapping = map (\x -> (x,())) keys
