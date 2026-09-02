{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem =
    {
      self',
      lib,
      config,
      pkgs,
      ...
    }:
    {
      haskellProjects.default = {
        projectRoot = builtins.toString (
          lib.fileset.toSource {
            inherit root;
            fileset = lib.fileset.unions [
              (root + /app)
              (root + /src)
              (root + /test)
              (root + /nix-sakuin.cabal)
              (root + /LICENSE)
              (root + /README.md)
            ];
          }
        );

        autoWire = [
          "packages"
          "apps"
          "checks"
        ];
      };

      packages.default = self'.packages.nix-sakuin;
      apps.default = self'.apps.nix-sakuin;
    };
}
