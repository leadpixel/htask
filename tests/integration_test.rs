use assert_cmd::Command;
use chrono::Utc;
use htask::domain::{Event, TaskIntent, TaskStatus, TaskUuid};
use htask::store::Store;
use predicates::prelude::*;
use tempfile::NamedTempFile;

#[test]
fn test_task_lifecycle() {
    let temp_file = NamedTempFile::new().unwrap();
    let file_path = temp_file.path().to_str().unwrap();

    let mut store = Store::load(file_path).unwrap();
    assert_eq!(store.tasks.len(), 0);

    let uuid1 = uuid::Uuid::new_v4();
    let event1 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::AddTask(TaskUuid(uuid1), "task 1".to_string()),
    };
    store.write_event(&event1).unwrap();
    assert_eq!(store.tasks.len(), 1);
    assert_eq!(store.tasks.get(&uuid1).unwrap().status, TaskStatus::Pending);

    let event2 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::StartTask(TaskUuid(uuid1)),
    };
    store.write_event(&event2).unwrap();
    assert_eq!(
        store.tasks.get(&uuid1).unwrap().status,
        TaskStatus::InProgress
    );

    let store2 = Store::load(file_path).unwrap();
    assert_eq!(store2.tasks.len(), 1);
    assert_eq!(
        store2.tasks.get(&uuid1).unwrap().status,
        TaskStatus::InProgress
    );
}

#[test]
fn test_cli_add_and_list() {
    let temp_file = NamedTempFile::new().unwrap();
    let file_path = temp_file.path().to_str().unwrap();

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_htask"));
    cmd.arg("--file")
        .arg(file_path)
        .arg("add")
        .arg("test task")
        .assert()
        .success()
        .stdout(predicate::str::contains("Success! added task: test task"));

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_htask"));
    cmd.arg("--file")
        .arg(file_path)
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("test task"))
        .stdout(predicate::str::contains("PENDING"));
}

#[test]
fn test_cli_json_output() {
    let temp_file = NamedTempFile::new().unwrap();
    let file_path = temp_file.path().to_str().unwrap();

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_htask"));
    cmd.arg("--file")
        .arg(file_path)
        .arg("add")
        .arg("json task")
        .assert()
        .success();

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_htask"));
    cmd.arg("--file")
        .arg(file_path)
        .arg("--json")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("\"description\": \"json task\""))
        .stdout(predicate::str::contains("\"status\": \"Pending\""));
}

#[test]
fn test_cli_empty_description_fails() {
    let temp_file = NamedTempFile::new().unwrap();
    let file_path = temp_file.path().to_str().unwrap();

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_htask"));
    cmd.arg("--file")
        .arg(file_path)
        .arg("add")
        .arg("")
        .assert()
        .failure()
        .stderr(predicate::str::contains("task description cannot be empty"));
}
