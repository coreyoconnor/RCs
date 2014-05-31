{ pkgs } :
{
  allowUnfree = true;

  packageOverrides = pkgs: {
      kde4 = pkgs.kde412;
      freetype = import (pkgs.path + "/pkgs/development/libraries/freetype") {
          inherit (pkgs) stdenv fetchurl gnumake;
          useInfinality = true;
      };

      wine = pkgs.misc.debugVersion (pkgs.stdenv.lib.overrideDerivation pkgs.wineUnstable (oldAttrs: {
        name = "wine-head";
        src = ../wine;
      }));
  };
}