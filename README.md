# nix-sakuin

A reimplementation of [`nix-index`](https://github.com/nix-community/nix-index) in Haskell, for better (or worse) performance.

## Building

```sh
$ nix build .
```

## Usage

Should be similar to how `nix-index` is used.

Generate a database:

```sh
$ nix-sakuin index
```

Locate a package:

```sh
$ nix-sakuin locate -t x --minimal /bin/ocamlopt
ocamlPackages.ocaml.out
```

For more information, see `nix-sakuin --help index`, and `nix-sakuin --help locate`.
