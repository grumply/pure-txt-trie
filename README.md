# pure-txt-trie

[Trie](https://en.wikipedia.org/wiki/Trie) for `Pure`'s `Txt` (`Text` on GHC and `JSString` on GHCJS).  This trie is specialized to Txt for performance reasons. 

Internally, `TxtTrie` is implemented as nested `IntMap`s.  Performance is generally improved for most operations compared with `Data.Map Txt a`.
