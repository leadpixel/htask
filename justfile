[private]
default:
    @just --list

# Build everything in release mode
build:
    cargo build --release

# Run all tests
test:
    cargo test

# Task file to use for 'run' target
tasks_file := ".tasks"

# Run the CLI app with arbitrary arguments (defaults to local .tasks)
run *args:
    @cargo run -- --file {{tasks_file}} {{args}}

# Install the CLI app to ~/.cargo/bin
install:
    cargo install --path .

# Run clippy on all source files
lint:
    cargo clippy

# Format all source files using rustfmt
format:
    cargo fmt

# Clean build artifacts
clean:
    cargo clean
    rm -f test.tasks

# Show instructions for enabling autocompletion
completion:
    @echo "htask uses clap for completions. To generate completions, use htask's built-in completion command if implemented, or use clap_complete in the build script."
    @echo "For now, refer to htask --help for available commands."
