[private]
default:
    @just --list

# Build all packages
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
    cabal install htask-cli --overwrite-policy=always

# Start a REPL for a specific package (default: htask-core)
repl package="htask-core":
    cabal repl {{package}}

# Run hlint on all source files
lint:
    hlint packages/

# Format all source files using stylish-haskell
format:
    find packages/ -name "*.hs" -exec stylish-haskell -i {} +

# Clean build artifacts
clean:
    cabal clean
    rm -rf dist-newstyle
