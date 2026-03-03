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

        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            let event: Event = serde_json::from_str(&line)?;
            tasks.apply_event(&event);
        }

        Ok(Store {
            file_path: file_path.as_ref().to_path_buf(),
            tasks,
        })
    }

    pub fn list_tasks(&self) -> Vec<Task> {
        let mut tasks: Vec<Task> = self.tasks.values().cloned().collect();
        tasks.sort(); // Uses Task's Ord implementation
        tasks
    }

    pub fn find_task(&self, ref_str: &str) -> Option<Task> {
        let sorted = self.list_tasks();

        if let Ok(n) = ref_str.parse::<usize>() {
            if (1..=sorted.len()).contains(&n) {
                return Some(sorted[n - 1].clone());
            }
        }

        sorted
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
    use crate::domain::TaskUuid;
    use chrono::Utc;

    #[test]
    fn test_task_map_events() {
        let mut tasks = TaskMap::new();
        let uuid1 = Uuid::new_v4();

        let event1 = Event {
            timestamp: Utc::now(),
            payload: TaskIntent::AddTask(TaskUuid(uuid1), "task 1".to_string()),
        };
        tasks.apply_event(&event1);

        let event2 = Event {
            timestamp: Utc::now(),
            payload: TaskIntent::StartTask(TaskUuid(uuid1)),
        };
        tasks.apply_event(&event2);

        assert_eq!(tasks.get(&uuid1).unwrap().status, TaskStatus::InProgress);
    }
}
