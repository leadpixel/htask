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
        exit 1
    fi
}

# Helper to get UUID of a task by description
get_uuid() {
    local desc="$1"
    $HTASK --file "$TEST_FILE" --json list --show-all | jq -r ".[] | select(.description == \"$desc\") | .taskUuid"
}

echo "--- README: Add ---"
run_htask add "some task description"
check_count 1
UUID=$(get_uuid "some task description")

echo "--- README: List ---"
run_htask list
run_htask ls
run_htask list --show-uuid
run_htask list --show-all

echo "--- README: Start (Numeric Index as per README) ---"
# README Example: htask start 1
run_htask start 1
run_htask --json list | jq -e ".[] | select(.taskUuid == \"$UUID\" and .status == \"InProgress\")" > /dev/null

echo "--- README: Stop (UUID) ---"
# Using UUID for reliability
run_htask stop "$UUID"
run_htask --json list | jq -e ".[] | select(.taskUuid == \"$UUID\" and .status == \"Pending\")" > /dev/null

echo "--- README: Complete (UUID) ---"
run_htask start "$UUID"
run_htask complete "$UUID"
check_count 0 # Hidden by default
check_count 1 --show-all

echo "--- README: Remove (UUID) ---"
run_htask add "abandon me"
AB_UUID=$(get_uuid "abandon me")
run_htask remove "$AB_UUID"
run_htask --json list --show-all | jq -e ".[] | select(.taskUuid == \"$AB_UUID\" and .status == \"Abandoned\")" > /dev/null

echo "--- README: Done ---"
run_htask add "task to be done"
DONE_UUID=$(get_uuid "task to be done")
run_htask start "$DONE_UUID"
run_htask done
# Both tasks should now be complete
COMPLETED_COUNT=$(run_htask --json list --show-all | jq '[.[] | select(.status == "Complete")] | length')
if [ "$COMPLETED_COUNT" -ne 2 ]; then
    echo "Error: Expected 2 completed tasks, but found $COMPLETED_COUNT"
    exit 1
fi

echo "--- README: Drop ---"
run_htask add "task to be dropped"
DROP_UUID=$(get_uuid "task to be dropped")
run_htask start "$DROP_UUID"
run_htask drop
run_htask --json list | jq -e ".[] | select(.taskUuid == \"$DROP_UUID\" and .status == \"Pending\")" > /dev/null

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
