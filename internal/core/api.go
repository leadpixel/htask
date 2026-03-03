package core

import (
	"errors"
	"strconv"
	"strings"
	"time"

	"github.com/google/uuid"
)

var (
	ErrTaskNotFound      = errors.New("unable to find matching task")
	ErrModifyFailed      = errors.New("unable to modify matching task")
	ErrEmptyDescription  = errors.New("task description cannot be empty")
	ErrFailedToAdd       = errors.New("failed to add")
)

type EventSource interface {
	ReadEvents() ([]Event, error)
}

type EventSink interface {
	WriteEvent(event Event) error
}

type TaskService struct {
	sink  EventSink
	Tasks map[uuid.UUID]*Task
}

func NewTaskService(sink EventSink, tasks map[uuid.UUID]*Task) *TaskService {
	return &TaskService{sink: sink, Tasks: tasks}
}

func NewTaskServiceFromSource(source EventSource, sink EventSink) (*TaskService, error) {
	evs, err := source.ReadEvents()
	if err != nil {
		return nil, err
	}
	tasks, _ := FoldEventLog(evs)
	return NewTaskService(sink, tasks), nil
}

func (s *TaskService) ListTasks() []*Task {
	var tasks []*Task
	for _, t := range s.Tasks {
		tasks = append(tasks, t)
	}
	return tasks
}

func (s *TaskService) FindTask(ref string) *Task {
	tasks := s.ListTasks()
	SortTasks(tasks)

	if n, err := strconv.Atoi(ref); err == nil && n > 0 && n <= len(tasks) {
		return tasks[n-1]
	}

	for _, t := range tasks {
		if strings.HasPrefix(t.TaskUUID.String(), ref) {
			return t
		}
	}
	return nil
}

func (s *TaskService) AddTask(description string) (*Task, error) {
	if strings.TrimSpace(description) == "" {
		return nil, ErrEmptyDescription
	}

	u := uuid.New()
	ev := Event{
		Timestamp: time.Now().UTC(),
		Payload: TaskIntent{
			Tag:         IntentAddTask,
			TaskUUID:    u,
			Description: description,
		},
	}

	success := applyEvent(s.Tasks, ev)
	if !success {
		return nil, ErrFailedToAdd
	}

	if s.sink != nil {
		if err := s.sink.WriteEvent(ev); err != nil {
			return nil, err
		}
	}
	return s.Tasks[u], nil
}

func (s *TaskService) StartTask(ref string) (*Task, error) {
	return s.modifyTask(ref, Pending, IntentStartTask)
}

func (s *TaskService) StopTask(ref string) (*Task, error) {
	return s.modifyTask(ref, InProgress, IntentStopTask)
}

func (s *TaskService) CompleteTask(ref string) (*Task, error) {
	return s.modifyTask(ref, InProgress, IntentCompleteTask)
}

func (s *TaskService) RemoveTask(ref string) (*Task, error) {
	t := s.FindTask(ref)
	if t == nil {
		return nil, ErrTaskNotFound
	}
	if t.Status == Complete || t.Status == Abandoned {
		return nil, ErrModifyFailed
	}

	ev := Event{
		Timestamp: time.Now().UTC(),
		Payload: TaskIntent{
			Tag:      IntentRemoveTask,
			TaskUUID: t.TaskUUID,
		},
	}
	if !applyEvent(s.Tasks, ev) {
		return nil, ErrModifyFailed
	}
	if s.sink != nil {
		if err := s.sink.WriteEvent(ev); err != nil {
			return nil, err
		}
	}
	return t, nil
}

func (s *TaskService) modifyTask(ref string, requiredStatus TaskStatus, intent TaskIntentType) (*Task, error) {
	t := s.FindTask(ref)
	if t == nil {
		return nil, ErrTaskNotFound
	}
	if t.Status != requiredStatus {
		return nil, ErrModifyFailed
	}

	ev := Event{
		Timestamp: time.Now().UTC(),
		Payload: TaskIntent{
			Tag:      intent,
			TaskUUID: t.TaskUUID,
		},
	}

	if !applyEvent(s.Tasks, ev) {
		return nil, ErrModifyFailed
	}

	if s.sink != nil {
		if err := s.sink.WriteEvent(ev); err != nil {
			return nil, err
		}
	}
	return t, nil
}
