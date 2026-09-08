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

## Notes

- I think the indexing speed is mostly dominated by network IO, so with the same
  (parallel) worker count, it may not be noticably faster or slower (I guess).
- The "database" is just a preformatted TSV file compressed with `zstd`. The "querying"
  is just decompressing and piping to `rg`, followed by a filter in Haskell. The speed is
  actually reasonable imo (except for the slow startup time of Haskell).
- Haskell indeed eats quite a lot of memory :)
