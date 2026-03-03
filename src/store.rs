use crate::domain::{Event, Task, TaskIntent, TaskStatus};
use anyhow::{Context, Result};
use std::collections::BTreeMap;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use uuid::Uuid;

#[derive(Debug, Default)]
pub struct TaskMap(BTreeMap<Uuid, Task>);

impl TaskMap {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn values(&self) -> impl Iterator<Item = &Task> {
        self.0.values()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, uuid: &Uuid) -> Option<&Task> {
        self.0.get(uuid)
    }

    pub fn apply_event(&mut self, event: &Event) -> Option<Event> {
        match &event.payload {
            TaskIntent::AddTask(ref_uuid, description) => {
                if self.0.contains_key(&ref_uuid.0)
                    || self.0.values().any(|t| t.description == *description)
                {
                    Some(event.clone())
                } else {
                    let task = Task {
                        task_uuid: *ref_uuid,
                        description: description.clone(),
                        created_at: event.timestamp,
                        status: TaskStatus::Pending,
                    };
                    self.0.insert(ref_uuid.0, task);
                    None
                }
            }
            TaskIntent::StartTask(ref_uuid) => {
                if let Some(task) = self.0.get_mut(&ref_uuid.0) {
                    if task.status == TaskStatus::Pending {
                        task.status = TaskStatus::InProgress;
                        None
                    } else {
                        Some(event.clone())
                    }
                } else {
                    Some(event.clone())
                }
            }
            TaskIntent::StopTask(ref_uuid) => {
                if let Some(task) = self.0.get_mut(&ref_uuid.0) {
                    if task.status == TaskStatus::InProgress {
                        task.status = TaskStatus::Pending;
                        None
                    } else {
                        Some(event.clone())
                    }
                } else {
                    Some(event.clone())
                }
            }
            TaskIntent::CompleteTask(ref_uuid) => {
                if let Some(task) = self.0.get_mut(&ref_uuid.0) {
                    if task.status == TaskStatus::InProgress {
                        task.status = TaskStatus::Complete;
                        None
                    } else {
                        Some(event.clone())
                    }
                } else {
                    Some(event.clone())
                }
            }
            TaskIntent::RemoveTask(ref_uuid) => {
                if let Some(task) = self.0.get_mut(&ref_uuid.0) {
                    if task.status != TaskStatus::Complete && task.status != TaskStatus::Abandoned {
                        task.status = TaskStatus::Abandoned;
                        None
                    } else {
                        Some(event.clone())
                    }
                } else {
                    Some(event.clone())
                }
            }
        }
    }
}

pub struct Store {
    pub file_path: PathBuf,
    pub tasks: TaskMap,
}

impl Store {
    pub fn load<P: AsRef<Path>>(file_path: P) -> Result<Self> {
        let file = File::open(&file_path)
            .with_context(|| format!("Failed to open {}", file_path.as_ref().display()))?;
        let reader = BufReader::new(file);
        let mut tasks = TaskMap::new();

        for (i, line) in reader.lines().enumerate() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            match serde_json::from_str::<Event>(&line) {
                Ok(event) => {
                    tasks.apply_event(&event);
                }
                Err(e) => {
                    eprintln!("Warning: Failed to decode event on line {}: {}", i + 1, e);
                }
            }
        }

        Ok(Store {
            file_path: file_path.as_ref().to_path_buf(),
            tasks,
        })
    }

    pub fn list_tasks(&self) -> Vec<Task> {
        let mut tasks: Vec<Task> = self.tasks.values().cloned().collect();
        tasks.sort_by(|a, b| a.display_order(b));
        tasks
    }

    pub fn priority_reference(&self) -> Vec<Task> {
        let mut tasks: Vec<Task> = self.tasks.values().cloned().collect();
        tasks.sort_by(|a, b| a.priority_order(b));
        tasks
    }

    pub fn find_task(&self, ref_str: &str) -> Option<Task> {
        let id_ref = self.priority_reference();

        if let Some(n) = ref_str
            .parse::<usize>()
            .ok()
            .filter(|&n| (1..=id_ref.len()).contains(&n))
        {
            return Some(id_ref[n - 1].clone());
        }

        self.list_tasks()
            .into_iter()
            .find(|t| t.task_uuid.0.to_string().starts_with(ref_str))
    }

    pub fn write_event(&mut self, event: &Event) -> Result<()> {
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&self.file_path)?;

        let json = serde_json::to_string(event)?;
        writeln!(file, "{}", json)?;

        self.tasks.apply_event(event);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{TaskIntent, TaskUuid};
    use chrono::Utc;

    #[test]
    fn test_task_map_add_task() {
        let mut tasks = TaskMap::new();
        let uuid = Uuid::new_v4();
        let event = Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid), "some task".to_string()),
        };

        let result = tasks.apply_event(&event);
        assert!(result.is_none());
        assert_eq!(tasks.len(), 1);

        let task = tasks.get(&uuid).unwrap();
        assert_eq!(task.description, "some task");
        assert_eq!(task.status, TaskStatus::Pending);
    }

    #[test]
    fn test_task_map_ignore_duplicate_tasks() {
        let mut tasks = TaskMap::new();
        let uuid1 = Uuid::new_v4();
        let event1 = Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid1), "duplicate".to_string()),
        };
        tasks.apply_event(&event1);

        let uuid2 = Uuid::new_v4();
        let event2 = Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid2), "duplicate".to_string()),
        };

        let result = tasks.apply_event(&event2);
        assert!(result.is_some());
        assert_eq!(tasks.len(), 1);
    }

    #[test]
    fn test_task_status_transitions() {
        let mut tasks = TaskMap::new();
        let uuid = Uuid::new_v4();
        let add_event = Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid), "task".to_string()),
        };
        tasks.apply_event(&add_event);

        // Start
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::StartTask(TaskUuid(uuid)),
        });
        assert_eq!(tasks.get(&uuid).unwrap().status, TaskStatus::InProgress);

        // Stop
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::StopTask(TaskUuid(uuid)),
        });
        assert_eq!(tasks.get(&uuid).unwrap().status, TaskStatus::Pending);

        // Start again then Complete
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::StartTask(TaskUuid(uuid)),
        });
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::CompleteTask(TaskUuid(uuid)),
        });
        assert_eq!(tasks.get(&uuid).unwrap().status, TaskStatus::Complete);

        // Remove (Abandon)
        let uuid2 = Uuid::new_v4();
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid2), "task 2".to_string()),
        });
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::RemoveTask(TaskUuid(uuid2)),
        });
        assert_eq!(tasks.get(&uuid2).unwrap().status, TaskStatus::Abandoned);
    }

    #[test]
    fn test_find_task_by_numeric_id() {
        let mut tasks = TaskMap::new();
        // Use fixed UUIDs that don't start with digits used in indices
        let uuid1 = Uuid::parse_str("aaaaaaaa-0000-0000-0000-000000000000").unwrap();
        let uuid2 = Uuid::parse_str("bbbbbbbb-0000-0000-0000-000000000000").unwrap();

        let now = Utc::now();
        tasks.apply_event(&Event {
            timestamp: now,
            payload: TaskIntent::AddTask(TaskUuid(uuid1), "task 1".to_string()),
        });
        tasks.apply_event(&Event {
            timestamp: now + chrono::Duration::seconds(1),
            payload: TaskIntent::AddTask(TaskUuid(uuid2), "task 2".to_string()),
        });

        let store = Store {
            file_path: PathBuf::from("fake"),
            tasks,
        };

        assert_eq!(store.find_task("1").unwrap().task_uuid.0, uuid1);
        assert_eq!(store.find_task("2").unwrap().task_uuid.0, uuid2);
        assert!(store.find_task("3").is_none());
    }

    #[test]
    fn test_find_task_by_uuid_prefix() {
        let mut tasks = TaskMap::new();
        let uuid = Uuid::parse_str("12345678-1234-1234-1234-123456789012").unwrap();
        tasks.apply_event(&Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid), "prefix test".to_string()),
        });

        let store = Store {
            file_path: PathBuf::from("fake"),
            tasks,
        };

        assert_eq!(store.find_task("1234").unwrap().task_uuid.0, uuid);
        assert!(store.find_task("5678").is_none());
    }

    #[test]
    fn test_task_sorting_display_order() {
        let mut tasks = TaskMap::new();
        let now = Utc::now();

        let uuid_pending = Uuid::new_v4();
        let uuid_in_progress = Uuid::new_v4();

        tasks.apply_event(&Event {
            timestamp: now,
            payload: TaskIntent::AddTask(TaskUuid(uuid_pending), "pending".to_string()),
        });
        tasks.apply_event(&Event {
            timestamp: now + chrono::Duration::seconds(1),
            payload: TaskIntent::AddTask(TaskUuid(uuid_in_progress), "in progress".to_string()),
        });
        tasks.apply_event(&Event {
            timestamp: now + chrono::Duration::seconds(1),
            payload: TaskIntent::StartTask(TaskUuid(uuid_in_progress)),
        });

        let store = Store {
            file_path: PathBuf::from("fake"),
            tasks,
        };

        let list = store.list_tasks();
        // InProgress should come before Pending
        assert_eq!(list[0].status, TaskStatus::InProgress);
        assert_eq!(list[1].status, TaskStatus::Pending);
    }

    #[test]
    fn test_load_invalid_json() {
        use std::io::Write;
        let mut temp = tempfile::NamedTempFile::new().unwrap();
        writeln!(temp, "invalid json").unwrap();
        writeln!(temp, "{{\"timestamp\": \"2026-03-03T08:00:00Z\", \"payload\": {{\"tag\": \"AddTask\", \"contents\": [\"00000000-0000-0000-0000-000000000000\", \"task\"]}}}}").unwrap();

        let store = Store::load(temp.path()).unwrap();
        // Should have skipped the invalid line and loaded the valid one
        assert_eq!(store.tasks.len(), 1);
    }
}
