{
  description = "plutus-simple-model";
  nixConfig = {
    # We don't use Recursive Nix yet.
    extra-experimental-features = [ "nix-command" "flakes" "recursive-nix" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]psm \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    plutarch.url = "github:Plutonomicon/plutarch?ref=01a67f56b2bf428538e92ed9ada0ce88d90ab636";
    ply.url = "github:mlabs-haskell/ply?ref=a7e3ea449c1f1770e046d0cda75d9bceb3582323";
    plutarch-quickcheck.url = "github:liqwid-labs/plutarch-quickcheck?ref=e6f0d2d6576932faaa08663d40ae1fb970634798";
    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra?ref=f9fa149db0b640c87268ee8865d4dd4175470937";
  };

  outputs = inputs@{ self, tooling, plutarch, ... }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;
          project.extraHackage = [
            "${plutarch}"
            "${inputs.plutarch}/plutarch-extra"
            "${inputs.ply}/ply-core"
            "${inputs.ply}/ply-plutarch"
            "${inputs.plutarch-quickcheck}"
            "${inputs.liqwid-plutarch-extra}"

          ];
          toHaddock = [
            "plutarch"
            "cardano-crypto"
            "cardano-ledger-alonzo"
            "cardano-ledger-babbage"
            "cardano-ledger-core"
          ];

        })
      ];
    };
}
