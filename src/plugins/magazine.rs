use super::{DisplayIcon, DisplayItem, MatchHandle, Plugin};
use crate::config::{send_notification, Config};
use gtk4::prelude::*;
use gtk4::Application;
use std::cell::RefCell;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;

pub struct MagazinePlugin {
    line_picker_state: Rc<RefCell<Option<PathBuf>>>,
    config: Rc<Config>,
}

impl MagazinePlugin {
    pub fn new(config: Rc<Config>) -> Self {
        let mag_dir = config.get_magazine_dir();
        if !mag_dir.exists() {
            let _ = fs::create_dir_all(&mag_dir);
        }
        Self {
            line_picker_state: Rc::new(RefCell::new(None)),
            config,
        }
    }

    fn get_magazine_path(&self, name: &str) -> Option<PathBuf> {
        if name.len() != 1 {
            return None;
        }
        let c = name.chars().next()?;
        let valid =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789[];',./{}:\"<>?#";
        if !valid.contains(c) {
            return None;
        }
        let path = self.config.get_magazine_dir().join(name);
        if !path.exists() {
            let _ = fs::write(&path, "");
        }
        Some(path)
    }

    fn get_lines(&self, path: &PathBuf) -> Vec<String> {
        let content = fs::read_to_string(path).unwrap_or_default();
        content
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.to_string())
            .collect()
    }
}

impl Plugin for MagazinePlugin {
    fn name(&self) -> &str {
        "magazine"
    }

    fn can_handle(&self, query: &str) -> bool {
        if self.line_picker_state.borrow().is_some() && query.is_empty() {
            *self.line_picker_state.borrow_mut() = None;
            return false;
        }

        if self.line_picker_state.borrow().is_some() {
            return true;
        }

        let query_lc = query.to_lowercase();

        if query_lc.len() == 1 && self.get_magazine_path(&query_lc).is_some() {
            return true;
        }

        if query.len() > 2 && query.chars().nth(1) == Some(' ') {
            let mag_char = &query[0..1];
            return self.get_magazine_path(mag_char).is_some();
        }

        false
    }

    fn search(&self, query: &str) -> Vec<(DisplayItem, MatchHandle)> {
        let query_lc = query.to_lowercase();

        if let Some(mag_path) = &*self.line_picker_state.borrow() {
            let lines = self.get_lines(mag_path);

            return lines
                .into_iter()
                .map(|line| {
                    let line_clone = line.clone();
                    (
                        DisplayItem {
                            label: line.clone(),
                            icon: DisplayIcon::named("text-x-generic"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |app: &Application| {
                            let _ = Command::new("wl-copy").arg(&line_clone).spawn();
                            send_notification("Magazine", "Line copied to clipboard");
                            app.quit();
                        }),
                    )
                })
                .collect();
        }

        if query_lc.len() == 1 {
            if let Some(mag_path) = self.get_magazine_path(&query_lc) {
                let mut results = Vec::new();

                {
                    let path = mag_path.clone();
                    let editor = self.config.get_editor().map(|s| s.to_string());
                    results.push((
                        DisplayItem {
                            label: "Edit in editor".to_string(),
                            icon: DisplayIcon::named("document-edit"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |app: &Application| {
                            if let Some(ed) = &editor {
                                let _ = Command::new(ed).arg(&path).spawn();
                            } else {
                                send_notification(
                                    "Config Error",
                                    "No editor configured in config.toml",
                                );
                            }
                            app.quit();
                        }),
                    ));
                }

                {
                    let path = mag_path.clone();
                    results.push((
                        DisplayItem {
                            label: "Copy to clipboard".to_string(),
                            icon: DisplayIcon::named("edit-copy"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |app: &Application| {
                            let content = fs::read_to_string(&path).unwrap_or_default();
                            let _ = Command::new("wl-copy").arg(&content).spawn();
                            send_notification("Magazine", "Copied to clipboard");
                            app.quit();
                        }),
                    ));
                }

                {
                    let path = mag_path.clone();
                    results.push((
                        DisplayItem {
                            label: "Paste from clipboard (overwrite)".to_string(),
                            icon: DisplayIcon::named("edit-paste"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |app: &Application| {
                            if let Ok(output) = Command::new("wl-paste").output() {
                                let content = String::from_utf8_lossy(&output.stdout).to_string();
                                let _ = fs::write(&path, content);
                                send_notification("Magazine", "Pasted from clipboard");
                            }
                            app.quit();
                        }),
                    ));
                }

                {
                    let path = mag_path.clone();
                    results.push((
                        DisplayItem {
                            label: "Append from clipboard".to_string(),
                            icon: DisplayIcon::named("list-add"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |app: &Application| {
                            if let Ok(output) = Command::new("wl-paste").output() {
                                let content = String::from_utf8_lossy(&output.stdout).to_string();
                                let mut existing = fs::read_to_string(&path).unwrap_or_default();
                                if !existing.is_empty() && !existing.ends_with('\n') {
                                    existing.push('\n');
                                }
                                existing.push_str(&content);
                                let _ = fs::write(&path, existing);
                                send_notification("Magazine", "Appended from clipboard");
                            }
                            app.quit();
                        }),
                    ));
                }

                {
                    let path = mag_path.clone();
                    let line_picker_state = self.line_picker_state.clone();
                    results.push((
                        DisplayItem {
                            label: "Pick a line".to_string(),
                            icon: DisplayIcon::named("edit-select-all"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |_app: &Application| {
                            *line_picker_state.borrow_mut() = Some(path.clone());
                        }),
                    ));
                }

                {
                    let path = mag_path.clone();
                    results.push((
                        DisplayItem {
                            label: "Clear magazine".to_string(),
                            icon: DisplayIcon::named("edit-clear"),
                            highlight_indices: None,
                        },
                        MatchHandle::new(move |app: &Application| {
                            let _ = fs::write(&path, "");
                            send_notification("Magazine", "Cleared");
                            app.quit();
                        }),
                    ));
                }

                return results;
            }
        }

        if query.len() > 2 && query.chars().nth(1) == Some(' ') {
            let mag_char = query[0..1].to_string();
            let text = query[2..].to_string();

            if let Some(mag_path) = self.get_magazine_path(&mag_char) {
                return vec![(
                    DisplayItem {
                        label: format!("Append to magazine {}: {}", mag_char, text),
                        icon: DisplayIcon::named("list-add"),
                        highlight_indices: None,
                    },
                    MatchHandle::new(move |app: &Application| {
                        let mut existing = fs::read_to_string(&mag_path).unwrap_or_default();
                        if !existing.is_empty() && !existing.ends_with('\n') {
                            existing.push('\n');
                        }
                        existing.push_str(&text);
                        let _ = fs::write(&mag_path, existing);
                        send_notification("Magazine", &format!("Appended to {}", mag_char));
                        app.quit();
                    }),
                )];
            }
        }

        Vec::new()
    }
}
