default:
    @just --list

docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

repl *ARGS:
    cabal repl {{ ARGS }}

run *ARGS:
    cabal exec nix-sakuin -- {{ ARGS }}

profile *ARGS:
    cabal exec -- nix-sakuin +RTS -hc -p -s -RTS {{ ARGS }}

clean:
    cabal clean && rm *.hp *.prof

build:
    cabal build && cabal test

build-profile:
    cabal build --enable-profiling --profiling-detail=late exe:nix-sakuin
