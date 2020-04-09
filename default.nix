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
      rev = "3166c357cd455607e4b5081a1019557e7bbf99f6";  # hoistFlowEff branch
      sha256 = "1ygds96ph6mdrjm8cmq7yrghp9f6y5kswii9p5jp2byg8n4p6z0a";
    };

    extendWithPorcupinePackages = self: _:
      pkgs.lib.mapAttrs (name: src: self.callCabal2nix name src {})
                        porcupineSources;

    inherit (pkgs.haskell.lib) doJailbreak dontCheck;
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

        funflow = dontCheck (self.callCabal2nix "funflow" "${funflowSource}/funflow" {});
                  # Check are failing for funflow, this should be investigated
      };
      }).extend extendWithPorcupinePackages;
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
