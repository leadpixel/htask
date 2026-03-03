#!/usr/bin/env bash
set -e

# Use the current cabal-run version of htask for testing
HTASK="cabal run htask --"
TEST_FILE=$(mktemp /tmp/htask-integration-XXXXXX.tasks)

echo "Using test file: $TEST_FILE"

# Function to run htask with the test file
run_htask() {
    $HTASK --file "$TEST_FILE" "$@"
}

cleanup() {
    rm -f "$TEST_FILE"
}
trap cleanup EXIT

# Helper to verify task count
check_count() {
    local expected=$1
    shift
    local count
    count=$($HTASK --file "$TEST_FILE" --json list "$@" | jq '. | length')
    if [ "$count" -ne "$expected" ]; then
        echo "Error: Expected $expected tasks, but found $count"
        $HTASK --file "$TEST_FILE" --json list "$@"
        exit 1
    fi
}

# Helper to get numeric ID of a task by description
get_id() {
    local desc="$1"
    # htask list output: "  01 ○ uuid \n       ╰─ description"
    # We use grep to find description, then look at the line ABOVE it.
    run_htask list --show-all | grep "$desc" -B 1 | head -n 1 | awk '{print $1}' | sed 's/^0*//'
}

echo "--- README: Add ---"
run_htask add "task one"
check_count 1

echo "--- README: List ---"
run_htask list > /dev/null
run_htask ls > /dev/null
run_htask list --show-uuid > /dev/null
run_htask list --show-all > /dev/null

echo "--- README: Start ---"
ID=$(get_id "task one")
run_htask start "$ID"
run_htask --json list | jq -e '.[] | select(.description == "task one" and .status == "InProgress")' > /dev/null

echo "--- README: Stop ---"
ID=$(get_id "task one")
run_htask stop "$ID"
run_htask --json list | jq -e '.[] | select(.description == "task one" and .status == "Pending")' > /dev/null

echo "--- README: Complete ---"
ID=$(get_id "task one")
run_htask start "$ID"
run_htask complete "$ID"
check_count 0 # Hidden by default
check_count 1 --show-all

echo "--- README: Remove ---"
run_htask add "abandon me"
ID=$(get_id "abandon me")
run_htask remove "$ID"
run_htask --json list --show-all | jq -e '.[] | select(.description == "abandon me" and .status == "Abandoned")' > /dev/null

echo "--- README: Done ---"
run_htask add "task to be done"
ID=$(get_id "task to be done")
run_htask start "$ID"
run_htask done
# Both 'task one' and 'task to be done' should now be complete
COMPLETED_COUNT=$(run_htask --json list --show-all | jq '[.[] | select(.status == "Complete")] | length')
if [ "$COMPLETED_COUNT" -ne 2 ]; then
    echo "Error: Expected 2 completed tasks, but found $COMPLETED_COUNT"
    run_htask --json list --show-all
    exit 1
fi

echo "--- README: Drop ---"
run_htask add "task to be dropped"
ID=$(get_id "task to be dropped")
run_htask start "$ID"
run_htask drop
run_htask --json list | jq -e '.[] | select(.description == "task to be dropped" and .status == "Pending")' > /dev/null

echo "--- README: Pick ---"
run_htask pick
# One task should now be in progress
run_htask --json list | jq -e '.[] | select(.status == "InProgress")' > /dev/null

echo "--- README: Summary ---"
run_htask summary > /dev/null
run_htask > /dev/null

echo "--- README: Global Options ---"
run_htask --help > /dev/null
$HTASK list --help > /dev/null
run_htask --version > /dev/null

echo "README integration tests passed!"
