use super::{DisplayIcon, DisplayItem, MatchHandle, Plugin};
use crate::config::{send_notification, Config};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use gtk4::prelude::*;
use gtk4::Application;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::rc::Rc;

#[derive(Clone)]
struct ScriptEntry {
    name: String,
    name_lower: String,
    path: PathBuf,
    shebang: String,
    open_in_terminal: bool,
}

pub struct ScriptsPlugin {
    matcher: SkimMatcherV2,
    scripts_dir: PathBuf,
    allowed_shebangs: Vec<String>,
    terminal: Option<String>,
}

impl ScriptsPlugin {
    pub fn new(config: Rc<Config>) -> Self {
        let scripts_dir = dirs::config_dir()
            .unwrap_or_else(|| {
                let home = std::env::var("HOME").expect("Could not get HOME env var");
                PathBuf::from(home).join(".config")
            })
            .join("launchery")
            .join("scripts");

        Self {
            matcher: SkimMatcherV2::default(),
            scripts_dir,
            allowed_shebangs: config.get_allowed_script_shebangs(),
            terminal: config.get_terminal().map(ToOwned::to_owned),
        }
    }

    fn scripts(&self) -> Vec<ScriptEntry> {
        let mut scripts: Vec<_> = fs::read_dir(&self.scripts_dir)
            .ok()
            .into_iter()
            .flat_map(|entries| entries.flatten())
            .filter_map(|entry| ScriptEntry::from_path(entry.path(), &self.allowed_shebangs))
            .collect();

        scripts.sort_by(|a, b| a.name_lower.cmp(&b.name_lower));
        scripts
    }

    fn matches_query(&self, query: &str) -> bool {
        if query.trim().is_empty() {
            return false;
        }

        let query = query.to_lowercase();
        self.scripts().iter().any(|script| {
            self.matcher
                .fuzzy_match(&script.name_lower, &query)
                .is_some()
        })
    }

    fn parse_query<'a>(&self, query: &'a str) -> (&'a str, bool) {
        if let Some(rest) = query.strip_prefix('#') {
            (rest.trim_start(), true)
        } else {
            (query, false)
        }
    }
}

impl ScriptEntry {
    fn from_path(path: PathBuf, allowed_shebangs: &[String]) -> Option<Self> {
        if path.extension().and_then(|ext| ext.to_str()) != Some("sh") {
            return None;
        }

        let content = fs::read_to_string(&path).ok()?;
        let mut lines = content.lines();
        let shebang = lines.next()?.trim();

        if !allowed_shebangs.iter().any(|allowed| allowed == shebang) {
            return None;
        }

        let name = content.lines().find_map(parse_name)?;
        let open_in_terminal = content.lines().find_map(parse_terminal).unwrap_or(false);

        if name.is_empty() {
            return None;
        }

        Some(Self {
            name_lower: name.to_lowercase(),
            name,
            path,
            shebang: shebang.to_string(),
            open_in_terminal,
        })
    }
}

fn parse_name(line: &str) -> Option<String> {
    line.trim()
        .strip_prefix("#NAME=")
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(ToOwned::to_owned)
}

fn parse_terminal(line: &str) -> Option<bool> {
    let line = line.trim();

    if line == "#TERMINAL" {
        return Some(true);
    }

    let value = line.strip_prefix("#TERMINAL=")?.trim();

    match value.to_lowercase().as_str() {
        "1" | "true" | "yes" | "on" => Some(true),
        "0" | "false" | "no" | "off" => Some(false),
        _ => None,
    }
}

fn run_script(path: &Path, shebang: &str, terminal: Option<&str>, app: &Application) {
    let parts: Vec<_> = shebang
        .trim()
        .trim_start_matches("#!")
        .split_whitespace()
        .collect();

    let Some(cmd) = parts.first() else {
        send_notification("Script Error", &format!("Invalid shebang in {:?}", path));
        app.quit();
        return;
    };

    let result = if let Some(terminal) = terminal {
        let mut command = parts.join(" ");
        command.push(' ');
        command.push_str(&shell_quote_path(path));

        Command::new(terminal)
            .arg("-e")
            .arg("sh")
            .arg("-c")
            .arg(command)
            .spawn()
    } else {
        Command::new(cmd).args(&parts[1..]).arg(path).spawn()
    };

    if let Err(e) = result {
        eprintln!("Failed to run script {:?}: {}", path, e);
        send_notification(
            "Script Error",
            &format!("Failed to run script {:?}: {}", path, e),
        );
    }

    app.quit();
}

fn shell_quote_path(path: &Path) -> String {
    let path = path.to_string_lossy();
    format!("'{}'", path.replace('\'', "'\\''"))
}

impl Plugin for ScriptsPlugin {
    fn name(&self) -> &str {
        "scripts"
    }

    fn can_handle(&self, query: &str) -> bool {
        let (filter, has_prefix) = self.parse_query(query);
        if has_prefix {
            return true;
        }
        self.matches_query(filter)
    }

    fn search(&self, query: &str) -> Vec<(DisplayItem, MatchHandle)> {
        let (filter, _) = self.parse_query(query);
        let filter = filter.to_lowercase();
        let scripts = self.scripts();

        let mut results: Vec<(i64, ScriptEntry, Option<Vec<usize>>)> = if filter.is_empty() {
            scripts
                .into_iter()
                .enumerate()
                .map(|(i, script)| (-(i as i64), script, None))
                .collect()
        } else {
            scripts
                .into_iter()
                .filter_map(|script| {
                    self.matcher
                        .fuzzy_indices(&script.name_lower, &filter)
                        .map(|(score, indices)| (score, script, Some(indices)))
                })
                .collect()
        };

        results.sort_unstable_by(|a, b| b.0.cmp(&a.0));

        results
            .into_iter()
            .map(|(_, script, indices)| {
                let name = script.name;
                let path = script.path;
                let shebang = script.shebang;
                let terminal = script
                    .open_in_terminal
                    .then(|| self.terminal.clone())
                    .flatten();

                (
                    DisplayItem {
                        label: name,
                        icon: DisplayIcon::named("text-x-script"),
                        highlight_indices: indices,
                    },
                    MatchHandle::new(move |app: &Application| {
                        run_script(&path, &shebang, terminal.as_deref(), app)
                    }),
                )
            })
            .collect()
    }
}
