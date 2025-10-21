{ pkgs, ... }:
{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc9103;
  };

  packages = [
    pkgs.libevdev
  ];

  scripts = {
    build.exec = "cabal build";
    run.exec = "cabal run";
    "run:T420:keyboard".exec = "cabal exec example -- /dev/input/by-path/platform-i8042-serio-0-event-kbd";
    "run:T420:trackpad".exec = "cabal exec example -- /dev/input/by-path/platform-i8042-serio-1-event-mouse";
    "run:T420:trackpoint".exec = "cabal exec example -- /dev/input/by-path/platform-i8042-serio-2-event-mouse";
    tests.exec = "cabal test";
    format.exec = "ormolu --mode inplace $(find {app,test} -name '*.hs')";
    lint.exec = "hlint {app,test}";
  };

  git-hooks.hooks = {
    ormolu.enable = true;
    hlint.enable = true;
  };
}
