use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum TaskStatus {
    InProgress,
    Pending,
    Complete,
    Abandoned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TaskUuid(pub Uuid);

impl From<Uuid> for TaskUuid {
    fn from(uuid: Uuid) -> Self {
        TaskUuid(uuid)
    }
}

impl fmt::Display for TaskUuid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Task {
    pub task_uuid: TaskUuid,
    pub description: String,
    pub created_at: DateTime<Utc>,
    pub status: TaskStatus,
}

impl PartialEq for Task {
    fn eq(&self, other: &Self) -> bool {
        self.task_uuid == other.task_uuid
    }
}

impl Eq for Task {}

impl Ord for Task {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.status
            .cmp(&other.status)
            .then(self.created_at.cmp(&other.created_at))
    }
}

impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum TaskIntent {
    AddTask(TaskUuid, String),
    StartTask(TaskUuid),
    StopTask(TaskUuid),
    CompleteTask(TaskUuid),
    RemoveTask(TaskUuid),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    pub timestamp: DateTime<Utc>,
    pub payload: TaskIntent,
}

impl Event {
    pub fn new(intent: TaskIntent) -> Self {
        Self {
            timestamp: Utc::now(),
            payload: intent,
        }
    }
}
