[private]
default:
    @just --list

# Build all packages
build:
    cabal build all

# Install the CLI app to ~/.local/bin
install:
    cabal install htask-cli --overwrite-policy=always

# Run all tests
test:
    cabal test all

# Run the CLI app
run *args:
    cabal run htask -- {{args}}

# Pin dependencies
freeze:
    cabal freeze
