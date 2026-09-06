{
  perSystem = { config, pkgs, ... }: {
    # Default shell.
    devShells.default = pkgs.mkShell {
      name = "nix-sakuin";
      meta.description = "Haskell development environment";

      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
      ];

      buildInputs = with pkgs; [
        pkg-config
        ripgrep
        zstd
        xz
      ];

      packages = with pkgs; [
        just
        nil
      ];
    };
  };
}
