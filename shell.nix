{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "MultiKill";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ random attoparsec text ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    description = "Combine multiple imputation models for great justice.";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
