# nix-sakuin

A reimplementation of [`nix-index`](https://github.com/nix-community/nix-index) in Haskell,
for better (or worse) performance.

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

FrCode is the default and uses `database.frcode.zst`. For TSV databases, select
`--db-format tsv` when building or querying `database.tsv.zst`. Both files can
coexist in the same database directory.

```sh
$ nix-sakuin index --db-format tsv
$ nix-sakuin locate --db-format tsv -t x --minimal /bin/ocamlopt
```

Select a FrCode query implementation with `--backend`. TSV always uses its
own search and prints a warning for non-default backend selections.

| Backend | Query implementation |
| --- | --- |
| `default` | Parse strict FrCode chunks into a reusable path buffer. This is also used when `--backend` is omitted. |
| `parallel` | Decode and match FrCode chunks across GHC capabilities while preserving database order. |
| `pure` | Use the immutable FrCode decoder and `Text` matching. |
| `cheat` | Use the single-threaded C backend with POSIX extended regular expressions for `--regex`. |

```sh
$ nix-sakuin locate --backend parallel -t x --minimal /bin/ocamlopt
```

## Benchmark

```sh
$ nix develop -c cabal bench rpackages
```

The benchmark compares TSV, FrCode, and upstream `nix-index` on `rPackages.jsonlite`,
`rPackages.digest`, `rPackages.rlang`, and their dependencies for `x86_64-linux`.
It times two fresh builds per backend at zstd level 3 with the fetch caches disabled.
Criterion measures a hit and a miss through the CLI. All FrCode query modes
share an index. The results directory contains the indexing timings, logs, and
Criterion report.

## Notes

For indexing:

- The indexing pipeline roughly follows `nix-index`:
  1. Call `nix-env` to lookup all available (root) packages. And add the store paths (hashes) to the work queue.
  2. For each store path, we fetch narinfo and listings from hydra (if available).
     - For the references (dependencies) in narinfo, we add them back to the work queue.
     - For the (file) listings, we parse the paths and add them to the database.
  3. The indexing finishes when the work queue is empty.
- I think the indexing speed is mostly dominated by network IO, so with the same (parallel) worker count,
  it may not be noticably faster or slower (I guess).
- The `--fetch-cache` option caches the fetched narinfo and listings serialized in `cbor`. The cache will be
  loaded and updated in subsequent runs. But the whole cache is loaded into memory during indexing,
  so it would take up quite a lot of RAM (really)...
- Haskell indeed eats quite a lot of memory :)

For the database and querying:

- Both formats use `zstd` compression. TSV queries decompress and pipe to `rg`, followed by a filter in Haskell.
- FrCode stores package information once per store path and encodes file paths using shared prefixes.
  Queries decode and filter records as they stream from `zstd`. The format uses nix-sakuin package records
  around the front coding from [nix-index’s `frcode.rs`](https://github.com/NixOS/nix-index/blob/master/src/frcode.rs).

Few things todo:

- JSON parsing from `aeson` produces quite a lot of allocations, which could be optimized by using a custom parser,
  but I am not sure if it is really worth the effort either.
