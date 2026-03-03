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

# Show instructions for enabling autocompletion
completion:
    @echo "For Bash, add this to your .bashrc:"
    @echo '  eval "$$(htask --bash-completion-script htask)"'
    @echo ""
    @echo "For Zsh, the most robust way is to save the script to a file in your fpath."
    @echo "1. Create a directory for completions if you don't have one:"
    @echo "   mkdir -p ~/.zsh/completion"
    @echo "2. Add it to your fpath in .zshrc (before compinit):"
    @echo '   fpath=(~/.zsh/completion $$fpath)'
    @echo "3. Generate the completion script:"
    @echo "   htask --zsh-completion-script htask > ~/.zsh/completion/_htask"
    @echo "4. Ensure compinit is called in your .zshrc:"
    @echo "   autoload -U compinit && compinit"
