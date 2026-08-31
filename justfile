default:
    @just --list

docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

repl *ARGS:
    cabal repl {{ ARGS }}

run *ARGS:
    cabal exec nix-sakuin -- {{ ARGS }}