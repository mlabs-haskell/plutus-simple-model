{
  description = "plutus-simple-model";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]psm \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
  };

  outputs = { self, tooling }: tooling.mkHaskellFlake1 {
    project = {
      src = ./.;
    };
  };
}
