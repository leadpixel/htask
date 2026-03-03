package core

import (
	"encoding/json"
	"slices"
	"strings"
	"time"

	"github.com/google/uuid"
)

type TaskStatus string

const (
	InProgress TaskStatus = "InProgress"
	Pending    TaskStatus = "Pending"
	Complete   TaskStatus = "Complete"
	Abandoned  TaskStatus = "Abandoned"
)

type Task struct {
	TaskUUID    uuid.UUID  `json:"taskUuid"`
	Description string     `json:"description"`
	CreatedAt   time.Time  `json:"createdAt"`
	Status      TaskStatus `json:"status"`
}

type TaskIntentType string

const (
	IntentAddTask      TaskIntentType = "AddTask"
	IntentStartTask    TaskIntentType = "StartTask"
	IntentStopTask     TaskIntentType = "StopTask"
	IntentCompleteTask TaskIntentType = "CompleteTask"
	IntentRemoveTask   TaskIntentType = "RemoveTask"
)

type TaskIntent struct {
	Tag         TaskIntentType
	TaskUUID    uuid.UUID
	Description string // Only used for AddTask
}

func (t *TaskIntent) UnmarshalJSON(data []byte) error {
	var raw struct {
		Tag      TaskIntentType  `json:"tag"`
		Contents json.RawMessage `json:"contents"`
	}
	if err := json.Unmarshal(data, &raw); err != nil {
		return err
	}
	t.Tag = raw.Tag
	switch t.Tag {
	case IntentAddTask:
		var contents []string
		if err := json.Unmarshal(raw.Contents, &contents); err != nil {
			return err
		}
		if len(contents) >= 2 {
			u, err := uuid.Parse(contents[0])
			if err != nil {
				return err
			}
			t.TaskUUID = u
			t.Description = contents[1]
		}
	default:
		var contentStr string
		if err := json.Unmarshal(raw.Contents, &contentStr); err != nil {
			return err
		}
		u, err := uuid.Parse(contentStr)
		if err != nil {
			return err
		}
		t.TaskUUID = u
	}
	return nil
}

func (t TaskIntent) MarshalJSON() ([]byte, error) {
	if t.Tag == IntentAddTask {
		return json.Marshal(struct {
			Tag      TaskIntentType `json:"tag"`
			Contents []string       `json:"contents"`
		}{
			Tag:      t.Tag,
			Contents: []string{t.TaskUUID.String(), t.Description},
		})
	}
	return json.Marshal(struct {
		Tag      TaskIntentType `json:"tag"`
		Contents string         `json:"contents"`
	}{
		Tag:      t.Tag,
		Contents: t.TaskUUID.String(),
	})
}

type Event struct {
	Timestamp time.Time  `json:"timestamp"`
	Payload   TaskIntent `json:"payload"`
}

func FoldEventLog(events []Event) (map[uuid.UUID]*Task, []Event) {
	taskMap := make(map[uuid.UUID]*Task)
	var failedEvents []Event

	for _, ev := range events {
		success := applyEvent(taskMap, ev)
		if !success {
			failedEvents = append(failedEvents, ev)
		}
	}
	return taskMap, failedEvents
}

func applyEvent(taskMap map[uuid.UUID]*Task, ev Event) bool {
	switch ev.Payload.Tag {
	case IntentAddTask:
		for _, t := range taskMap {
			if t.Description == ev.Payload.Description || t.TaskUUID == ev.Payload.TaskUUID {
				return false
			}
		}
		taskMap[ev.Payload.TaskUUID] = &Task{
			TaskUUID:    ev.Payload.TaskUUID,
			Description: ev.Payload.Description,
			CreatedAt:   ev.Timestamp,
			Status:      Pending,
		}
		return true
	case IntentStartTask:
		if t, ok := taskMap[ev.Payload.TaskUUID]; ok {
			t.Status = InProgress
			return true
		}
		return false
	case IntentStopTask:
		if t, ok := taskMap[ev.Payload.TaskUUID]; ok {
			t.Status = Pending
			return true
		}
		return false
	case IntentCompleteTask:
		if t, ok := taskMap[ev.Payload.TaskUUID]; ok {
			t.Status = Complete
			return true
		}
		return false
	case IntentRemoveTask:
		if t, ok := taskMap[ev.Payload.TaskUUID]; ok {
			t.Status = Abandoned
			return true
		}
		return false
	}
	return false
}

func DisambiguatingPrefixes(uuids []uuid.UUID) map[uuid.UUID]string {
	strs := make([]string, len(uuids))
	for i, u := range uuids {
		strs[i] = u.String()
	}

	res := make(map[uuid.UUID]string)
	for _, u := range uuids {
		t := u.String()
		found := t
		for n := 4; n <= 36; n++ {
			if n > len(t) {
				break
			}
			p := t[:n]
			count := 0
			for _, str := range strs {
				if strings.HasPrefix(str, p) {
					count++
				}
			}
			if count == 1 {
				found = p
				break
			}
		}
		res[u] = found
	}
	return res
}

func SortTasks(tasks []*Task) {
	order := map[TaskStatus]int{
		InProgress: 0,
		Pending:    1,
		Complete:   2,
		Abandoned:  3,
	}
	slices.SortStableFunc(tasks, func(a, b *Task) int {
		if a.Status != b.Status {
			return order[a.Status] - order[b.Status]
		}
		return a.CreatedAt.Compare(b.CreatedAt)
	})
}
