[private]
default:
    @just --list

# Build everything
build:
    cabal build all

# Run all tests
test:
    cabal test all

# Task file to use for 'run' target
tasks_file := ".tasks"

# Run the CLI app with arbitrary arguments (defaults to local .tasks)
run *args:
    @cabal run htask -- --file {{tasks_file}} {{args}}

# Pin dependencies
freeze:
    cabal freeze

# Install the CLI app to ~/.cabal/bin
install:
    cabal install exe:htask --overwrite-policy=always

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

# Show instructions for enabling Bash autocompletion
completion:
    @echo "To enable Bash autocompletion, add the following to your .bashrc:"
    @echo 'eval \"\$(htask --bash-completion-script htask)\"'
