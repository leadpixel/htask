# htask


## Desired behaviour

- addition: `task add buy eggs`
- listing: `task list`

## Git Integration

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
