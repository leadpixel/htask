package main

import (
	"fmt"
	"math/rand"
	"os"
	"strings"

	"github.com/google/uuid"
	"github.com/leadpixel/htask/internal/cli"
	"github.com/leadpixel/htask/internal/core"
	"github.com/leadpixel/htask/internal/events"
	"github.com/spf13/cobra"
)

var (
	jsonFlag bool
	fileFlag string
	showUUID bool
	showAll  bool
)

func initService() *core.TaskService {
	path := fileFlag
	if path == "" {
		path = events.ResolveDefaultPath()
	}

	if _, err := os.Stat(path); os.IsNotExist(err) {
		os.WriteFile(path, []byte{}, 0644)
	}

	backend := events.NewFileBackend(path)
	svc, err := core.NewTaskServiceFromSource(backend, backend)
	if err != nil {
		cli.ErrorResult{err.Error()}.Render()
		os.Exit(1)
	}
	return svc
}

func formatTaskEntry(t *core.Task, allTasks []*core.Task, prefixes map[uuid.UUID]string) []string {
	idx := "??"
	for i, at := range allTasks {
		if at.TaskUUID == t.TaskUUID {
			idx = fmt.Sprintf("%02d", i+1)
			break
		}
	}
	symbol := cli.StatusSymbol(t.Status)
	uuidText := t.TaskUUID.String()
	if p, ok := prefixes[t.TaskUUID]; ok && !showUUID {
		uuidText = p
	}

	headerLine := fmt.Sprintf("  %s %s", cli.WithStatusColor(t.Status, fmt.Sprintf("%s %s", idx, symbol)), cli.WithDim(uuidText))
	
	lines := strings.Split(t.Description, "\n")
	descLines := []string{}
	if len(lines) > 0 {
		descLines = append(descLines, "       "+cli.TreeLink()+cli.WithBold(lines[0]))
		for _, l := range lines[1:] {
			descLines = append(descLines, "          "+cli.WithBold(l))
		}
	}
	return append([]string{headerLine}, descLines...)
}

func main() {
	rootCmd := &cobra.Command{
		Use:   "htask",
		Short: "track tasks in a local event log",
		Run: func(cmd *cobra.Command, args []string) {
			runSummary()
		},
	}

	rootCmd.PersistentFlags().BoolVarP(&jsonFlag, "json", "j", false, "output in JSON format")
	rootCmd.PersistentFlags().StringVarP(&fileFlag, "file", "f", "", "path to the task list file (default: .tasks or ~/.tasks)")

	addCmd := &cobra.Command{
		Use:   "add [DESCRIPTION]",
		Short: "Add a task description",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			t, err := svc.AddTask(args[0])
			if err != nil {
				cli.ErrorResult{err.Error()}.Render()
				os.Exit(1)
			}
			if jsonFlag {
				cli.JsonResult{Value: t.TaskUUID}.Render()
			} else {
				cli.SuccessResult{
					cli.WithStatusColor(core.Pending, "added task: ")+cli.WithBold(t.Description),
					cli.WithDim("      ref: "+t.TaskUUID.String()),
				}.Render()
			}
		},
	}

	startCmd := &cobra.Command{
		Use:   "start [TASKREF]",
		Short: "Starts a given task",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			t, err := svc.StartTask(args[0])
			if err != nil {
				cli.ErrorResult{err.Error()}.Render()
				os.Exit(1)
			}
			if jsonFlag {
				cli.JsonResult{Value: t}.Render()
			} else {
				cli.SuccessResult{cli.WithStatusColor(core.InProgress, "starting task: ")+cli.WithBold(t.Description)}.Render()
			}
		},
	}

	stopCmd := &cobra.Command{
		Use:   "stop [TASKREF]",
		Short: "Stops a given task",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			t, err := svc.StopTask(args[0])
			if err != nil {
				cli.ErrorResult{err.Error()}.Render()
				os.Exit(1)
			}
			if jsonFlag {
				cli.JsonResult{Value: t}.Render()
			} else {
				cli.SuccessResult{cli.WithStatusColor(core.Pending, "stopping task: ")+cli.WithBold(t.Description)}.Render()
			}
		},
	}

	completeCmd := &cobra.Command{
		Use:   "complete [TASKREF]",
		Short: "Mark a task as completed",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			t, err := svc.CompleteTask(args[0])
			if err != nil {
				cli.ErrorResult{err.Error()}.Render()
				os.Exit(1)
			}
			if jsonFlag {
				cli.JsonResult{Value: t}.Render()
			} else {
				cli.SuccessResult{cli.WithStatusColor(core.Complete, "completing task: ")+cli.WithBold(t.Description)}.Render()
			}
		},
	}

	removeCmd := &cobra.Command{
		Use:   "remove [TASKREF]",
		Short: "Removes a task from the list",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			t, err := svc.RemoveTask(args[0])
			if err != nil {
				cli.ErrorResult{err.Error()}.Render()
				os.Exit(1)
			}
			if jsonFlag {
				cli.JsonResult{Value: t}.Render()
			} else {
				cli.SuccessResult{cli.WithStatusColor(core.Abandoned, "removing task: ")+cli.WithBold(t.Description)}.Render()
			}
		},
	}

	listCmd := &cobra.Command{
		Use:   "list",
		Short: "List tasks; optionally show removed tasks",
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			tasks := svc.ListTasks()
			core.SortTasks(tasks)

			var filtered []*core.Task
			for _, t := range tasks {
				if showAll || t.Status == core.Pending || t.Status == core.InProgress {
					filtered = append(filtered, t)
				}
			}

			if jsonFlag {
				cli.JsonResult{Value: filtered}.Render()
				return
			}

			uuids := make([]uuid.UUID, len(tasks))
			for i, t := range tasks {
				uuids[i] = t.TaskUUID
			}
			prefixes := core.DisambiguatingPrefixes(uuids)

			out := []string{}
			for _, st := range []core.TaskStatus{core.InProgress, core.Pending, core.Complete, core.Abandoned} {
				var grp []*core.Task
				for _, t := range filtered {
					if t.Status == st {
						grp = append(grp, t)
					}
				}
				if len(grp) > 0 {
					header := "PENDING"
					switch st {
					case core.InProgress: header = "IN PROGRESS"
					case core.Complete: header = "COMPLETED"
					case core.Abandoned: header = "ABANDONED"
					}
					out = append(out, "\n"+cli.Divider(header))
					for _, t := range grp {
						out = append(out, formatTaskEntry(t, tasks, prefixes)...)
					}
				}
			}
			cli.SuccessResult(out).Render()
		},
	}
	listCmd.Flags().BoolVarP(&showUUID, "show-uuid", "u", false, "Show UUID for tasks")
	listCmd.Flags().BoolVarP(&showAll, "show-all", "a", false, "Include completed and removed tasks")
	listCmd.Aliases = append(listCmd.Aliases, "ls")

	pickCmd := &cobra.Command{
		Use:   "pick",
		Short: "Picks the next task by priority",
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			tasks := svc.ListTasks()
			var pendings []*core.Task
			for _, t := range tasks {
				if t.Status == core.Pending {
					pendings = append(pendings, t)
				}
			}
			if len(pendings) == 0 {
				cli.ErrorResult{"no task to pick"}.Render()
				os.Exit(1)
			}
			idx := rand.Intn(len(pendings))
			t := pendings[idx]
			t, err := svc.StartTask(t.TaskUUID.String())
			if err != nil {
				cli.ErrorResult{err.Error()}.Render()
				os.Exit(1)
			}
			if jsonFlag {
				cli.JsonResult{Value: t}.Render()
			} else {
				cli.SuccessResult{cli.WithStatusColor(core.InProgress, "picking task: ")+cli.WithBold(t.Description)}.Render()
			}
		},
	}

	dropCmd := &cobra.Command{
		Use:   "drop",
		Short: "Drops the current tasks in progress",
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			tasks := svc.ListTasks()
			var outs []string
			var ts []*core.Task
			for _, t := range tasks {
				if t.Status == core.InProgress {
					nt, err := svc.StopTask(t.TaskUUID.String())
					if err == nil {
						ts = append(ts, nt)
						outs = append(outs, cli.WithStatusColor(core.Pending, "stopping task: ")+cli.WithBold(nt.Description))
					} else {
						outs = append(outs, "unable to modify matching task")
					}
				}
			}
			if jsonFlag {
				cli.JsonResult{Value: ts}.Render()
			} else {
				cli.SuccessResult(outs).Render()
			}
		},
	}

	doneCmd := &cobra.Command{
		Use:   "done",
		Short: "Marks the current task as completed",
		Run: func(cmd *cobra.Command, args []string) {
			svc := initService()
			tasks := svc.ListTasks()
			var outs []string
			var ts []*core.Task
			for _, t := range tasks {
				if t.Status == core.InProgress {
					nt, err := svc.CompleteTask(t.TaskUUID.String())
					if err == nil {
						ts = append(ts, nt)
						outs = append(outs, cli.WithStatusColor(core.Complete, "completing task: ")+cli.WithBold(nt.Description))
					} else {
						outs = append(outs, "unable to modify matching task")
					}
				}
			}
			if jsonFlag {
				cli.JsonResult{Value: ts}.Render()
			} else {
				cli.SuccessResult(outs).Render()
			}
		},
	}

	summaryCmd := &cobra.Command{
		Use:   "summary",
		Short: "A short summary of current tasks",
		Run: func(cmd *cobra.Command, args []string) {
			runSummary()
		},
	}

	rootCmd.AddCommand(addCmd, startCmd, stopCmd, completeCmd, removeCmd, listCmd, pickCmd, dropCmd, doneCmd, summaryCmd)
	rootCmd.Execute()
}

func runSummary() {
	svc := initService()
	tasks := svc.ListTasks()
	core.SortTasks(tasks)

	var actives []*core.Task
	var pendings []*core.Task

	for _, t := range tasks {
		if t.Status == core.InProgress {
			actives = append(actives, t)
		} else if t.Status == core.Pending {
			pendings = append(pendings, t)
		}
	}

	topPendings := pendings
	if len(pendings) > 5 {
		topPendings = pendings[:5]
	}

	uuids := make([]uuid.UUID, len(tasks))
	for i, t := range tasks {
		uuids[i] = t.TaskUUID
	}
	prefixes := core.DisambiguatingPrefixes(uuids)

	if jsonFlag {
		res := append(actives, topPendings...)
		if res == nil {
			res = []*core.Task{}
		}
		cli.JsonResult{Value: res}.Render()
		return
	}

	out := []string{}
	if len(actives) == 0 {
		out = append(out, "No current task")
	} else {
		out = append(out, cli.Divider("CURRENT TASK"))
		for _, t := range actives {
			out = append(out, formatTaskEntry(t, tasks, prefixes)...)
		}
	}

	msg := fmt.Sprintf("PENDING TASKS (%d OF %d)", len(topPendings), len(pendings))
	out = append(out, "\n"+cli.Divider(msg))
	for _, t := range topPendings {
		out = append(out, formatTaskEntry(t, tasks, prefixes)...)
	}

	cli.SuccessResult(out).Render()
}
