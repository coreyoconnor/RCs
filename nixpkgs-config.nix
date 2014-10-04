{ pkgs } :
{
  allowUnfree = true;

  packageOverrides = pkgs: {
      kde4 = pkgs.kde414;

      wine = pkgs.misc.debugVersion (pkgs.stdenv.lib.overrideDerivation pkgs.wineUnstable (oldAttrs: {
        name = "wine-head";
        src = ../wine;
      }));
  };
}
