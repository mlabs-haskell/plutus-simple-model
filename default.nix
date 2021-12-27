{ sourcesFile ? ./nix/sources.json, system ? builtins.currentSystem
, sources ? import ./nix/sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, plutus-apps ? import sources.plutus-apps { }
, deferPluginErrors ? true
, doCoverage ? true }:
let
  project = import ./nix/haskell.nix {
    inherit sourcesFile system sources plutus plutus-apps deferPluginErrors doCoverage;
  };
in project
