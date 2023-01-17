Plutus simple model
====================================================

Unit test library for Plutus with estimation of resource usage.

Library defines simple mock model of Blockchain to unit test plutus contracts
and estimate usage of resources. What are the benefits for this framework comparing
to other test frameworks for Plutus? It's:

* easy to use
* easy to think about
* it has minimal dependency set. It depends only on plutus and cardano-ledger.
    Ain't no plutus-apps or cardano-node or ouroboros
* works for both Plutus V1 and Plutus V2
* support for Alonzo and Babbage eras
* blazing fast
* can estimate usage of resources with real functions used on Cardano node.
* pure
* good for unit testing of on-chain code
* being fast and pure it's also suitable for property based testing

## Quick start guide

See [user guide](https://mlabs-haskell.github.io/plutus-simple-model/) to get started

## How to compile the library

Library is compiled with nix, flakes and cabal. Enter developer nix shell with:

```
nix develop
```

Inside the nix shell we can run build and tests over make:

```
make build
make test
```

It calls cabal under the hood.

## How to update docs

Docs are written in [mdbook](https://github.com/rust-lang/mdBook) and reside in the `docs` directory.
They are auto-deployed with Git actions on push to main. To run docs locally use mdbook:

```
cd docs
mdbook serve
```

## Known issues

### `nix develop` haddock phase fails

#### Using `mlabs-tooling.nix`

Add following snippet to `mkHaskellFlakeModule1`

```nix
mkHaskellFlakeModule1 {
  # ...
  project.modules = [({ config, ... }: {
    packages.plutus-simple-model.doHaddock = false;
  })];
  # ...
};

```

#### Using plain `haskell.nix`

Add following snippet to `cabalProject'`

```nix
pkgs.haskell-nix.cabalProject' {
  # ...
  modules = [
    ({ config, ... }: {
      packages.plutus-simple-model.doHaddock = false;
    })
  ];
  # ...
};
```
