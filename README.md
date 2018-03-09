# HTask

A command-line utility for task management

## Motivation

I work on the command line. I want my task management to be handled there too.

I want a simple task list to keep me focused on the next most important thing.

I want the tasks to be co-located with the code.

I want multiple users to be able to work on tasks in a repo without conflict.

And I want to keep a history of task changes to understand how I and collaborators are working.

## Setup

- install [Stack](https://docs.haskellstack.org/en/stable/README/)
- checkout the repo and verify:
```sh
git clone https://github.com/leadpixel/htask.git
cd htask
stack test --pedantic
```

- build and install:
```sh
stack install
```

You should see output like `Copied executables to /some/local/path`; check this is on your `$PATH`

- first run:
```sh
touch .tasks
htask
```

You should see output like `No current task`; then you're good to go!

## Global Options

`-h, --help` Lists all available commands with a short description of each; is always more up-to-date than this document
`-f, --file ARG`: path to a tasks file

```sh
htask --help # for top-level help
htask [CMD] --help # for command-specific help (replace [CMD] with list, for example)
```

## Commands

### Summary

The default; gets invoked when you run `htask` without an argument
Prints the current tasks and the top pending tasks

```sh
htask
htask summary
```

### List

Prints a list of active tasks
Optionally shows deleted tasks
Optionally shows task ids

```sh
htask list
htask ls

htask list (-u|--show-uuid) # shows task id for use with other commands
htask list (-a|--all) # show all tasks, including deleted
```

### Add

Adds a task

```sh
htask add "some task description"
```

### Start

Starts a task selected by uuid
It's a pain to type out the uuid, so using [fzf](https://github.com/junegunn/fzf) can make this easier

```sh

htask start SOME_LONG_UUID
htask start $(htask list --show-uuid | fzf --ansi | cut -f 1 -d " ")
```

### Stop

Stops a task selected by uuid
It's a pain to type out the uuid, so using [fzf](https://github.com/junegunn/fzf) can make this easier

```sh

htask stop SOME_LONG_UUID
htask stop $(htask list --show-uuid | fzf --ansi | cut -f 1 -d " ")
```

### Complete

### Remove

## Useful aliases
```sh
alias h=htask
alias g='htask --file ~/.tasks'
```

## Git Merges

Conflicts can occur when events are created in multiple branches. As the task list is an append-only log, we can simplify the merging behaviour. We know we want to keep all events, ordered by timestamp.

First, we need to configure a merge driver that will provide this behaviour:

```sh
git config merge.union.name "merge by union"
git config merge.union.driver "git merge-file --union -L %P %A %O %B"
```

Then we tell Git to use this driver for the task list file:

```sh
echo ".tasks merge=union" >> .gitattributes
```

Done! Now merges should not cause conflicts; if they do, please raise an [issue](https://github.com/leadpixel/htask/issues)
