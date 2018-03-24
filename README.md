# pure-txt-trie

[Trie](https://en.wikipedia.org/wiki/Trie) specialized to `Pure`'s `Txt` (`Text` on GHC and `JSString` on GHCJS; both unicode) for performance.

Internally, `TxtTrie` is implemented as nested `IntMap`s.  Performance is generally improved compared with `Data.Map.Map Txt a`; some operations can be quite a bit faster; improved memory consumption can often be observed.
