use chrono::Utc;
use htask::domain::{Event, TaskIntent, TaskStatus, TaskUuid};
use htask::store::Store;
use tempfile::NamedTempFile;

#[test]
fn test_task_lifecycle() {
    let temp_file = NamedTempFile::new().unwrap();
    let file_path = temp_file.path().to_str().unwrap();

    // 1. Initial Load
    // Store::load should not fail on empty file, but we should handle it
    // Actually, Store::load expects valid JSON lines. Empty file is fine (0 events).
    let mut store = Store::load(file_path).unwrap();
    assert_eq!(store.tasks.len(), 0);

    // 2. Add Task
    let uuid1 = uuid::Uuid::new_v4();
    let event1 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::AddTask(TaskUuid(uuid1), "task 1".to_string()),
    };
    store.write_event(&event1).unwrap();
    assert_eq!(store.tasks.len(), 1);
    assert_eq!(store.tasks.get(&uuid1).unwrap().status, TaskStatus::Pending);

    // 3. Start Task
    let event2 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::StartTask(TaskUuid(uuid1)),
    };
    store.write_event(&event2).unwrap();
    assert_eq!(
        store.tasks.get(&uuid1).unwrap().status,
        TaskStatus::InProgress
    );

    // 4. Stop Task
    let event3 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::StopTask(TaskUuid(uuid1)),
    };
    store.write_event(&event3).unwrap();
    assert_eq!(store.tasks.get(&uuid1).unwrap().status, TaskStatus::Pending);

    // 5. Complete Task
    let event4 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::StartTask(TaskUuid(uuid1)),
    };
    store.write_event(&event4).unwrap();
    let event5 = Event {
        timestamp: Utc::now(),
        payload: TaskIntent::CompleteTask(TaskUuid(uuid1)),
    };
    store.write_event(&event5).unwrap();
    assert_eq!(
        store.tasks.get(&uuid1).unwrap().status,
        TaskStatus::Complete
    );

    // 6. Reload Store
    let store2 = Store::load(file_path).unwrap();
    assert_eq!(store2.tasks.len(), 1);
    assert_eq!(
        store2.tasks.get(&uuid1).unwrap().status,
        TaskStatus::Complete
    );
}
