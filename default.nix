/*
Workflow:

### Global

```
$ nix-shell
```

This will open a nix-shell with all the dependencies for all the
porcupine packages. This will also generates the cabal files and
cabal.project needed for all subproject.

```
$ cabal new-repl porcupine-s3
```

will open a ghci with porcupine-s3 in scope.

If you change anything to a dependency, you must reload the cabal
new-repl!

### Local

```
# Create a shell with all dependencies for porcupine-s3
nix-shell -A porcupine-s3.env

cd porcupine-s3

# build the cabal file
hpack

# start a repl
cabal new-repl
```

A few notes:

- You must restart your nix-shell to reload dependencies. For example,
  if you change something in docrecords when working in the
  porcupine-http shell, changes in docrecords and porcupine-core will
  be accounted for on next nix-shell restart
- Nix caching is based on directory content, so any leftover file
  (such as the generated .cabal) will invalidate it. This can be
  extended by source filtering
*/

{ withHoogle ? false }:

let
porcupineSources = {
  docrecords = ./docrecords;
  reader-soup = ./reader-soup;
  porcupine-core = ./porcupine-core;
  porcupine-s3 = ./porcupine-s3;
  porcupine-http = ./porcupine-http;
};

overlayHaskell = _:pkgs:
  let
    funflowSource = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "funflow";
      rev = "v1.5.0";
      sha256 = "1bldpi1fcr6l7qcr7kkwycvp1i9jgpd6b8m4190z9lsiddz0pkav";
    };
    monadBayesSource = pkgs.fetchFromGitHub {
      owner = "tweag"; # Using our fork until https://github.com/adscib/monad-bayes/pull/54 is merged
      repo = "monad-bayes";
      rev = "ffd695379fefc056e99c43e3ca4d79be9a6372af";
      sha256 = "1qikvzpkpm255q3mgpc0x9jipxg6kya3pxgkk043vk25h2j11l0p";
    };

    inherit (pkgs.haskell.lib) doJailbreak dontCheck packageSourceOverrides;
  in {
  haskellPackages =
    (pkgs.haskellPackages.override {
      overrides = self: super: rec {
        streaming-conduit = doJailbreak super.streaming-conduit;

        # hedis checks take too long, so they are disabled:
        hedis = dontCheck super.hedis;
        # vinyl hasn't been updated for hspec >= 2.7:
        vinyl = dontCheck super.vinyl;
        hvega = super.hvega_0_4_1_0;
        monad-bayes = self.callCabal2nix "monad-bayes" monadBayesSource {};

        funflow = dontCheck (self.callCabal2nix "funflow" "${funflowSource}/funflow" {});
                  # Check are failing for funflow, this should be investigated
      };
      }).extend (packageSourceOverrides porcupineSources);
};

# Nixpkgs clone
pkgs = import ./nixpkgs.nix {
  config = {
    allowBroken = true; # for a few packages, such as streaming-conduit
  };

  overlays = [ overlayHaskell ];
};

porcupinePkgs = builtins.mapAttrs (x: _: pkgs.haskellPackages.${x}) porcupineSources;

# The final shell
shell = pkgs.haskellPackages.shellFor {
  packages = _: builtins.attrValues porcupinePkgs;
  nativeBuildInputs = [pkgs.cabal-install];
  withHoogle = withHoogle;
  # Generates the cabal and cabal project file
  shellHook =
  ''
    echo "packages:" > cabal.project
    for i in ${toString (builtins.attrNames porcupineSources)}
    do
      hpack $i
      echo "    $i" >> cabal.project
    done
    '';
};

# this derivation contains the shell and all the porcupine package in
# direct access
in
{
  inherit shell;
} // porcupinePkgs
