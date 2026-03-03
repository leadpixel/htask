package events

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/leadpixel/htask/internal/core"
)

type EventSource interface {
	ReadEvents() ([]core.Event, error)
}

type EventSink interface {
	WriteEvent(event core.Event) error
}

type FileBackend struct {
	path string
}

func NewFileBackend(path string) *FileBackend {
	return &FileBackend{path: path}
}

func ResolveDefaultPath() string {
	cwd, err := os.Getwd()
	if err != nil {
		home, _ := os.UserHomeDir()
		return filepath.Join(home, ".tasks")
	}

	dirs := strings.Split(cwd, string(filepath.Separator))
	for i := len(dirs); i > 0; i-- {
		path := filepath.Join("/", filepath.Join(dirs[:i]...), ".tasks")
		if _, err := os.Stat(path); err == nil {
			return path
		}
	}

	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".tasks")
}

func (b *FileBackend) ReadEvents() ([]core.Event, error) {
	file, err := os.Open(b.path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}
	defer file.Close()

	var events []core.Event
	scanner := bufio.NewScanner(file)
	lineNum := 1
	for scanner.Scan() {
		line := scanner.Bytes()
		if len(line) == 0 {
			lineNum++
			continue
		}
		var ev core.Event
		if err := json.Unmarshal(line, &ev); err != nil {
			fmt.Fprintf(os.Stderr, "Warning: Failed to decode event on line %d: %s\n", lineNum, string(line))
		} else {
			events = append(events, ev)
		}
		lineNum++
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return events, nil
}

func (b *FileBackend) WriteEvent(event core.Event) error {
	file, err := os.OpenFile(b.path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	data, err := json.Marshal(event)
	if err != nil {
		return err
	}
	data = append(data, '\n')
	_, err = file.Write(data)
	return err
}
