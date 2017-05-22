{ reflex-platform ? import ./reflex-platform {}
, base-platform ? reflex-platform.ghcjs
, ...
}:

with {
  pkgs = reflex-platform.nixpkgs;
  inherit (reflex-platform.nixpkgs) fetchgitPrivate fetchFromGitHub;
  inherit (reflex-platform.nixpkgs.haskell.lib) dontCheck;
  inherit (builtins) fromJSON readFile;
};

base-platform.override {
  overrides = self: super: {
  };
}
