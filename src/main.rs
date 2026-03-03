use anyhow::{Context, Result, bail};
use clap::{Parser, Subcommand};
use colored::*;
use htask::domain::{Event, TaskIntent, TaskStatus, TaskUuid};
use htask::store::Store;
use rand::seq::IndexedRandom;
use std::collections::HashMap;
use std::path::PathBuf;
use uuid::Uuid;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Path to the task list file (default: .tasks or ~/.tasks)
    #[arg(short, long, global = true)]
    file: Option<PathBuf>,

    /// Output in JSON format
    #[arg(short, long, global = true)]
    json: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Add a task description
    Add { description: String },
    /// Mark a task as completed
    Complete { task_ref: String },
    /// Marks the current task as completed
    Done,
    /// Drops the current tasks in progress
    Drop,
    /// List tasks; optionally show removed tasks
    List {
        #[arg(short = 'u', long)]
        show_uuid: bool,
        #[arg(short = 'a', long)]
        show_all: bool,
    },
    /// Picks the next task by priority
    Pick,
    /// Removes a task from the list
    Remove { task_ref: String },
    /// Starts a given task
    Start { task_ref: String },
    /// Stops a given task
    Stop { task_ref: String },
    /// A short summary of current tasks
    Summary,
}

fn resolve_default_path() -> PathBuf {
    let mut current = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    loop {
        let tasks_file = current.join(".tasks");
        if tasks_file.exists() {
            return tasks_file;
        }
        if let Some(parent) = current.parent() {
            current = parent.to_path_buf();
        } else {
            break;
        }
    }
    dirs::home_dir()
        .map(|h| h.join(".tasks"))
        .unwrap_or_else(|| PathBuf::from(".tasks"))
}

fn disambiguating_prefixes(tasks: &[htask::domain::Task]) -> HashMap<TaskUuid, String> {
    let mut prefixes = HashMap::new();
    let uuids: Vec<String> = tasks
        .iter()
        .map(|t| t.task_uuid.0.to_string())
        .collect::<Vec<_>>();

    for task in tasks {
        let full = task.task_uuid.0.to_string();
        let mut prefix = full.clone();
        for n in 4..=full.len() {
            let p = &full[..n];
            if uuids.iter().filter(|u| u.starts_with(p)).count() == 1 {
                prefix = p.to_string();
                break;
            }
        }
        prefixes.insert(task.task_uuid, prefix);
    }
    prefixes
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Set color override based on terminal presence
    colored::control::set_override(std::io::stdout().is_terminal());

    let file_path = cli.file.unwrap_or_else(resolve_default_path);

    if !file_path.exists() {
        if std::io::stdin().is_terminal() {
            print!(
                "Task file '{}' not found. Create it? [y/N] ",
                file_path.display()
            );
            use std::io::Write;
            std::io::stdout().flush()?;
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;
            if input.trim().to_lowercase() == "y" {
                std::fs::File::create(&file_path)?;
            } else {
                eprintln!("Aborted.");
                std::process::exit(1);
            }
        } else {
            eprintln!("Task file '{}' not found.", file_path.display());
            std::process::exit(1);
        }
    }

    let mut store = Store::load(&file_path)?;

    let command = cli.command.unwrap_or(Commands::Summary);

    match command {
        Commands::Add { description } => {
            if description.trim().is_empty() {
                bail!("task description cannot be empty");
            }
            let uuid = Uuid::new_v4();
            let event = Event::new(TaskIntent::AddTask(TaskUuid(uuid), description.clone()));
            store.write_event(&event)?;
            println!("{} added task: {}", "Success!".green(), description.bold());
            println!("      {} {}", "ref:".dimmed(), uuid.to_string().dimmed());
        }
        Commands::Start { task_ref } => {
            let task = store
                .find_task(&task_ref)
                .context("unable to find matching task")?;
            store.write_event(&Event::new(TaskIntent::StartTask(task.task_uuid)))?;
            println!(
                "{} starting task: {}",
                "Success!".green(),
                task.description.bold()
            );
        }
        Commands::Stop { task_ref } => {
            let task = store
                .find_task(&task_ref)
                .context("unable to find matching task")?;
            store.write_event(&Event::new(TaskIntent::StopTask(task.task_uuid)))?;
            println!(
                "{} stopping task: {}",
                "Success!".green(),
                task.description.bold()
            );
        }
        Commands::Complete { task_ref } => {
            let task = store
                .find_task(&task_ref)
                .context("unable to find matching task")?;
            store.write_event(&Event::new(TaskIntent::CompleteTask(task.task_uuid)))?;
            println!(
                "{} completing task: {}",
                "Success!".green(),
                task.description.bold()
            );
        }
        Commands::Remove { task_ref } => {
            let task = store
                .find_task(&task_ref)
                .context("unable to find matching task")?;
            store.write_event(&Event::new(TaskIntent::RemoveTask(task.task_uuid)))?;
            println!(
                "{} removing task: {}",
                "Success!".green(),
                task.description.bold()
            );
        }
        Commands::Done => {
            let in_progress: Vec<_> = store
                .tasks
                .values()
                .filter(|t| t.status == TaskStatus::InProgress)
                .cloned()
                .collect();
            for task in in_progress {
                store.write_event(&Event::new(TaskIntent::CompleteTask(task.task_uuid)))?;
                println!(
                    "{} completing task: {}",
                    "Success!".green(),
                    task.description.bold()
                );
            }
        }
        Commands::Drop => {
            let in_progress: Vec<_> = store
                .tasks
                .values()
                .filter(|t| t.status == TaskStatus::InProgress)
                .cloned()
                .collect();
            for task in in_progress {
                store.write_event(&Event::new(TaskIntent::StopTask(task.task_uuid)))?;
                println!(
                    "{} stopping task: {}",
                    "Success!".green(),
                    task.description.bold()
                );
            }
        }
        Commands::Pick => {
            let pending: Vec<_> = store
                .tasks
                .values()
                .filter(|t| t.status == TaskStatus::Pending)
                .cloned()
                .collect();
            if let Some(task) = pending.choose(&mut rand::rng()) {
                store.write_event(&Event::new(TaskIntent::StartTask(task.task_uuid)))?;
                println!(
                    "{} picking task: {}",
                    "Success!".green(),
                    task.description.bold()
                );
            } else {
                println!("Error: no task to pick");
            }
        }
        Commands::List {
            show_uuid,
            show_all,
        } => {
            let tasks = store.list_tasks();
            let filtered: Vec<_> = tasks
                .into_iter()
                .filter(|t| {
                    show_all
                        || t.status == TaskStatus::InProgress
                        || t.status == TaskStatus::Pending
                })
                .collect();

            if cli.json {
                println!("{}", serde_json::to_string_pretty(&filtered)?);
            } else {
                let id_ref = store.priority_reference();
                let prefixes = disambiguating_prefixes(&id_ref);
                render_tasks(&filtered, &id_ref, &prefixes, show_uuid);
            }
        }
        Commands::Summary => {
            let tasks = store.list_tasks();
            let in_progress: Vec<_> = tasks
                .iter()
                .filter(|t| t.status == TaskStatus::InProgress)
                .collect();
            let pending: Vec<_> = tasks
                .iter()
                .filter(|t| t.status == TaskStatus::Pending)
                .collect::<Vec<_>>();

            if cli.json {
                let summary: Vec<_> = in_progress
                    .iter()
                    .cloned()
                    .cloned()
                    .chain(pending.iter().take(5).cloned().cloned())
                    .collect();
                println!("{}", serde_json::to_string_pretty(&summary)?);
            } else {
                let id_ref = store.priority_reference();
                let prefixes = disambiguating_prefixes(&id_ref);

                if in_progress.is_empty() {
                    println!("No current task");
                } else {
                    println!("{}", divider("CURRENT TASK"));
                    for t in in_progress.iter() {
                        print_task(t, &id_ref, &prefixes, false);
                    }
                }

                let total_pending = pending.len();
                let top_pending = pending.iter().take(5).collect::<Vec<_>>();
                println!(
                    "\n{}",
                    divider(&format!(
                        "PENDING TASKS ({} OF {})",
                        top_pending.len(),
                        total_pending
                    ))
                );
                for t in top_pending.iter() {
                    print_task(t, &id_ref, &prefixes, false);
                }
            }
        }
    }

    Ok(())
}

fn divider(label: &str) -> String {
    format!(
        "{} {}",
        label.bold(),
        "─"
            .repeat(60_usize.saturating_sub(label.len() + 1))
            .dimmed()
    )
}

fn render_tasks(
    tasks: &[htask::domain::Task],
    id_ref: &[htask::domain::Task],
    prefixes: &HashMap<TaskUuid, String>,
    show_uuid: bool,
) {
    let mut current_status = None;
    for task in tasks {
        if Some(task.status) != current_status {
            current_status = Some(task.status);
            let header = match task.status {
                TaskStatus::InProgress => "IN PROGRESS",
                TaskStatus::Pending => "PENDING",
                TaskStatus::Complete => "COMPLETED",
                TaskStatus::Abandoned => "ABANDONED",
            };
            println!("\n{}", divider(header));
        }
        print_task(task, id_ref, prefixes, show_uuid);
    }
}

fn print_task(
    task: &htask::domain::Task,
    id_ref: &[htask::domain::Task],
    prefixes: &HashMap<TaskUuid, String>,
    show_uuid: bool,
) {
    let symbol = match task.status {
        TaskStatus::InProgress => "▶".cyan(),
        TaskStatus::Pending => "○".yellow(),
        TaskStatus::Complete => "✔".green(),
        TaskStatus::Abandoned => "✘".red(),
    };

    let idx = id_ref
        .iter()
        .position(|t| t.task_uuid == task.task_uuid)
        .map(|i| i + 1)
        .unwrap_or(0);

    let idx_str = format!("{:02}", idx);
    let status_color = match task.status {
        TaskStatus::InProgress => idx_str.cyan(),
        TaskStatus::Pending => idx_str.yellow(),
        TaskStatus::Complete => idx_str.green(),
        TaskStatus::Abandoned => idx_str.red(),
    };

    let uuid_str = if show_uuid {
        task.task_uuid.0.to_string()
    } else {
        prefixes
            .get(&task.task_uuid)
            .cloned()
            .unwrap_or_else(|| task.task_uuid.0.to_string())
    };

    println!("  {} {} {}", status_color, symbol, uuid_str.dimmed());

    let lines: Vec<_> = task.description.lines().collect();
    if !lines.is_empty() {
        println!("       ╰─ {}", lines[0].bold());
        for line in lines.iter().skip(1) {
            println!("          {}", line.bold());
        }
    }
}

use std::io::IsTerminal;
