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

# Run the CLI app with arbitrary arguments
run *args:
    cabal run htask -- {{args}}

# Pin dependencies
freeze:
    cabal freeze

# Show a summary of tasks
summary:
    @cabal run htask -- summary

# List all tasks
ls *args:
    @cabal run htask -- list {{args}}

# List all tasks with UUIDs
lsu *args:
    @cabal run htask -- list --show-uuid {{args}}

# Add a new task
add description:
    @cabal run htask -- add "{{description}}"

# Start a task
start taskid:
    @cabal run htask -- start {{taskid}}

# Stop a task
stop taskid:
    @cabal run htask -- stop {{taskid}}

# Complete a task
complete taskid:
    @cabal run htask -- complete {{taskid}}

# Remove a task
remove taskid:
    @cabal run htask -- remove {{taskid}}

# Pick a task (shortcut)
pick:
    @cabal run htask -- pick

# Drop the current task (shortcut)
drop:
    @cabal run htask -- drop

# Mark the current task as done (shortcut)
done:
    @cabal run htask -- done
