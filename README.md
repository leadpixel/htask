# HTask

A command-line utility for task management.

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
$ git clone https://github.com/leadpixel/htask.git
$ cd htask
$ stack test --pedantic
```

- build and install:
```sh
$ stack install
```

You should see output like `Copied executables to /some/local/path`; check this is on your `$PATH`

- first run:
```sh
$ touch .tasks
$ htask
```

You should see output like `No current task`; then you're good to go!

## Global Options

`-h, --help` Lists all available commands with a short description of each; is always more up-to-date than this document
`-f, --file ARG`: path to a tasks file

```sh
$ htask --help # for top-level help
$ htask [CMD] --help # for command-specific help (replace [CMD] with list, for example)
```

## Commands

### Summary

The default; gets invoked when you run `htask` without an argument.

Prints the current tasks and the top pending tasks.

```sh
$ htask
$ htask summary
```

### List

Prints a list of active tasks.

Optionally shows completed and removed tasks.

Optionally shows task ids.

```sh
$ htask list
$ htask ls

$ htask list (-u|--show-uuid) # shows task id for use with other commands
$ htask list (-a|--show-all) # show all tasks (including hidden)
```

### Add

Adds a task; requires a task description.

Task descriptions can be empty strings and can contain newlines.

```sh
$ htask add "some task description"
# Success!
# added task: some task description
```

### Start

Starts a task selected by uuid.

See [Selecting Tasks](#selecting-tasks) for a handy shortcut.

```sh
$ htask start SOME_TASK_UUID
# Success!
# starting task: some task description
```

### Stop

Stops a task selected by uuid.

See [Selecting Tasks](#selecting-tasks) for a handy shortcut.

```sh
$ htask stop SOME_TASK_UUID
# Success!
# stopping task: some task description
```

### Complete

Marks a task as completed.

See [Selecting Tasks](#selecting-tasks) for a handy shortcut.

```sh
$ htask complete SOME_TASK_UUID
# Success!
# completing task: some task description
```

### Remove

Marks a task as abandoned.

See [Selecting Tasks](#selecting-tasks) for a handy shortcut.

```sh
$ htask remove SOME_TASK_UUID
# Success!
# removing task: some task description
```

## Tips
### Selecting Tasks

It's a pain to type out the uuid, so using [fzf](https://github.com/junegunn/fzf) can make this easier
```sh
$ htask [CMD] $(htask list --show-uuid | fzf --ansi | cut -f 1 -d " ")
```

This is easier with an alias:
```sh
$ alias hpick="htask list --show-uuid | fzf --ansi | cut -f 1 -d ' '"
$ htask [CMD] $(hpick)
```

### Global Tasks

It can be helpful to have project-level tasks as well as global-level tasks. By specifying the location of a task file, you can create distinct sets of tasks.

```sh
$ alias g='htask --file ~/.tasks'
```

### Git Merges

Conflicts can occur when events are created in multiple branches. As the task list is an append-only log, we can simplify the merging behaviour. We know we want to keep all events, ordered by timestamp.

First, we need to configure a merge driver that will provide this behaviour:

```sh
$ git config merge.union.name "merge by union"
$ git config merge.union.driver "git merge-file --union -L %P %A %O %B"
```

Then we tell Git to use this driver for the task list file:

```sh
$ echo ".tasks merge=union" >> .gitattributes
```

Done! Now merges should not cause conflicts; if they do, please raise an [issue](https://github.com/leadpixel/htask/issues)
