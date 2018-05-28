{ mkDerivation, base, containers, deepseq, pure-json, pure-txt
, stdenv
}:
mkDerivation {
  pname = "pure-txt-trie";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq pure-json pure-txt
  ];
  license = stdenv.lib.licenses.bsd3;
}
