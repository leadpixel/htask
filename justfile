[private]
default:
    @just --list

# Build everything
build:
    cabal build all

# Run all tests
test:
    cabal test all

# Run the CLI app with arbitrary arguments
run *args:
    @cabal run htask -- {{args}}

# Pin dependencies
freeze:
    cabal freeze

# Install the CLI app to ~/.cabal/bin
install:
    cabal install htask --overwrite-policy=always

# Start a REPL for the library
repl:
    cabal repl htask

# Run hlint on all source files
lint:
    hlint src/ app/ test/

# Format all source files using stylish-haskell
format:
    find src/ app/ test/ -name "*.hs" -exec stylish-haskell -i {} +

# Clean build artifacts
clean:
    cabal clean
    rm -rf dist-newstyle
