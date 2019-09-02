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
    funflowRev = "f0e0238aba688637fb6487a7ff4e24f2ae312a1d";
    funflowSource = pkgs.fetchzip {
      url = "https://github.com/tweag/funflow/archive/${funflowRev}.tar.gz";
      sha256 = "0gxws140rk4aqy00zja527nv87bnm2blp8bikmdy4a2wyvyc7agv";
    };
    hvegaSource = pkgs.fetchzip {
      url = "http://hackage.haskell.org/package/hvega-0.3.0.1/hvega-0.3.0.1.tar.gz";
      sha256 = "03hnb7abk1jpiidabgdbvbdl736frgpzfk9wv94028i5vkxkgg1x";
    };

    inherit (pkgs.haskell.lib) doJailbreak dontCheck packageSourceOverrides;
  in {
  haskellPackages =
    (pkgs.haskellPackages.override {
      overrides = self: super: rec {

        # A few package version override.
        # Hopefully a future nixpkgs update will make them as default
        # so they will be in the binary cache.
        network-bsd = super.network-bsd_2_8_1_0;
        network = super.network_3_1_0_0;
        socks = super.socks_0_6_0;
        connection = super.connection_0_3_0;
        streaming-conduit = doJailbreak super.streaming-conduit;

        # checks take too long, so they are disabled
        hedis = dontCheck super.hedis_0_12_5;
        hvega = self.callCabal2nix "hvega" hvegaSource {};

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
  withHoogle = true;
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
