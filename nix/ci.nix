{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, plutus-apps ? import sources.plutus-apps { }
, deferPluginErrors ? true
, doCoverage ? false }:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus plutus-apps deferPluginErrors doCoverage;
  };
in rec {
  # What should CI build?

  inherit (project) projectCoverageReport;
  inherit (project.gero-gov.components) library;

  inherit (project.gero-gov.components.tests) gero-gov-test;

  # This will run the tests within this build and produce the test logs to stdout
  check = plutus-apps.pkgs.runCommand "run-tests" { } ''
    ${gero-gov-test}/bin/gero-gov-test
    echo "" > $out
  '';

}
