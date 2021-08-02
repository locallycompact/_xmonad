let pkgs = (import ./nix/base.nix).pkgs;
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "lc-xmonad";
    src = ./.;
  };
}
