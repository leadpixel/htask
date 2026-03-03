package cli

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/leadpixel/htask/internal/core"
	"golang.org/x/term"
)

var useColor = term.IsTerminal(int(os.Stdout.Fd()))

func WithColor(colorCode int, s string) string {
	if useColor {
		return fmt.Sprintf("\033[%dm%s\033[0m", colorCode, s)
	}
	return s
}

func WithBold(s string) string {
	if useColor {
		return fmt.Sprintf("\033[1m%s\033[0m", s)
	}
	return s
}

func WithDim(s string) string {
	return WithColor(90, s)
}

func WithStatusColor(status core.TaskStatus, s string) string {
	switch status {
	case core.InProgress:
		return WithColor(36, s) // Cyan
	case core.Pending:
		return WithColor(33, s) // Yellow
	case core.Complete:
		return WithColor(32, s) // Green
	case core.Abandoned:
		return WithColor(31, s) // Red
	default:
		return s
	}
}

func StatusSymbol(status core.TaskStatus) string {
	switch status {
	case core.InProgress:
		return "▶"
	case core.Pending:
		return "○"
	case core.Complete:
		return "✔"
	case core.Abandoned:
		return "✘"
	default:
		return "?"
	}
}

func Divider(label string) string {
	lineLen := 60 - len(label)
	if lineLen < 0 {
		lineLen = 0
	}
	line := ""
	for i := 0; i < lineLen; i++ {
		line += "─"
	}
	return WithBold(label) + " " + WithDim(line)
}

func TreeLink() string {
	return "╰─ "
}

type RunResult interface {
	Render()
}

type SuccessResult []string

func (r SuccessResult) Render() {
	fmt.Println(WithColor(32, "Success!"))
	for _, s := range r {
		fmt.Println(s)
	}
}

type ErrorResult []string

func (r ErrorResult) Render() {
	fmt.Println(WithColor(31, "Error") + ":")
	for _, s := range r {
		fmt.Println(s)
	}
}

type JsonResult struct {
	Value interface{}
}

func (r JsonResult) Render() {
	data, _ := json.MarshalIndent(r.Value, "", "    ")
	fmt.Println(string(data))
}
