# Nix tools for Gero Wallet Governance

This directory contains all of the nix helper functions used to build our
dependencies.

# Formatting

Use nixfmt (provided by the shell) to format the nix sources.

# Niv dependency pinning

Use `niv` to update / modify nix dependencies.

# Updating plutus

In order to update the plutus revision, a few steps must be taken:

- lookup for dependencies in the repo input-output-hk/plutus-apps in the file `cabal.project`.
  Update local gero `cabal.project` according to the dependencies that are required by the package plutus-apps.
  Note that also not only hashes get update but also there might be some new deps and the
  underlying structure for certain dep can become different.

- `niv update plutus -r <revision>` will update Nix's plutus revision which will
  also produce a new shell

  We use it to setup all new dependencies or update old ones to the required revisions.
  Revision is called tag in the `cabal.project`

Now everything should be updated, good luck fixing compile errors!
