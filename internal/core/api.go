package core

import (
	"errors"
	"strconv"
	"strings"
	"time"

	"github.com/google/uuid"
)

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
		return nil, errors.New("task description cannot be empty")
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
		return nil, errors.New("failed to add")
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
		return nil, errors.New("unable to find matching task")
	}
	if t.Status == Complete || t.Status == Abandoned {
		return nil, errors.New("unable to modify matching task")
	}

	ev := Event{
		Timestamp: time.Now().UTC(),
		Payload: TaskIntent{
			Tag:      IntentRemoveTask,
			TaskUUID: t.TaskUUID,
		},
	}
	if !applyEvent(s.Tasks, ev) {
		return nil, errors.New("unable to modify matching task")
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
		return nil, errors.New("unable to find matching task")
	}
	if t.Status != requiredStatus {
		return nil, errors.New("unable to modify matching task")
	}

	ev := Event{
		Timestamp: time.Now().UTC(),
		Payload: TaskIntent{
			Tag:      intent,
			TaskUUID: t.TaskUUID,
		},
	}

	if !applyEvent(s.Tasks, ev) {
		return nil, errors.New("unable to modify matching task")
	}

	if s.sink != nil {
		if err := s.sink.WriteEvent(ev); err != nil {
			return nil, err
		}
	}
	return t, nil
}
