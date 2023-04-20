{
  description = "plutus-simple-model";
  nixConfig = {
    # We don't use Recursive Nix yet.
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" "recursive-nix" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]psm \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
  };

  outputs = inputs@{ self, tooling, nixpkgs, ... }:
    tooling.lib.mkFlake { inherit self; }
      {
        imports = [
          (tooling.lib.mkHaskellFlakeModule1 {
            project.src = ./.;
            project.extraHackage = [
            ];
            toHaddock = [
              "cardano-crypto"
              "cardano-ledger-alonzo"
              "cardano-ledger-babbage"
              "cardano-ledger-core"
            ];

          })
        ];
      };
}
