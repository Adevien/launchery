use super::{DisplayIcon, DisplayItem, MatchHandle, Plugin};
use crate::config::{send_notification, Config};
use crate::plugins::app_launcher::AppLauncherPlugin;
use gtk4::prelude::*;
use gtk4::Application;
use std::process::Command;
use std::rc::Rc;

pub struct ActionsPlugin {
    inner: Rc<AppLauncherPlugin>,
    config: Rc<Config>,
}

impl ActionsPlugin {
    pub fn new(inner: Rc<AppLauncherPlugin>, config: Rc<Config>) -> Self {
        Self { inner, config }
    }
}

impl Plugin for ActionsPlugin {
    fn name(&self) -> &str {
        "app_launcher_with_actions"
    }

    fn can_handle(&self, query: &str) -> bool {
        self.inner.can_handle(query)
    }

    fn search(&self, query: &str) -> Vec<(DisplayItem, MatchHandle)> {
        self.inner.search(query)
    }

    fn get_actions(&self, _query: &str, item_index: usize) -> Vec<(DisplayItem, MatchHandle)> {
        let (app_name, exec_parts) = match self.inner.get_app_info(item_index) {
            Some(info) => info,
            None => return Vec::new(),
        };

        let cmd_string = exec_parts.join(" ");

        vec![
            {
                let exec = exec_parts.clone();
                let name = app_name.clone();
                (
                    DisplayItem {
                        label: format!("Launch {}", app_name),
                        icon: DisplayIcon::named("system-run"),
                        highlight_indices: None,
                    },
                    MatchHandle::new(move |app: &Application| {
                        if let Some(cmd) = exec.first() {
                            if let Err(e) = Command::new(cmd).args(&exec[1..]).spawn() {
                                eprintln!("Failed to launch {}: {}", name, e);
                                send_notification(
                                    "Launch Error",
                                    &format!("Failed to launch {}: {}", name, e),
                                );
                            }
                        }
                        app.quit();
                    }),
                )
            },
            {
                let exec = exec_parts.clone();
                let name = app_name.clone();
                let terminal = self.config.get_terminal().map(|s| s.to_string());
                (
                    DisplayItem {
                        label: format!("Launch {} in terminal", app_name),
                        icon: DisplayIcon::named("utilities-terminal"),
                        highlight_indices: None,
                    },
                    MatchHandle::new(move |app: &Application| {
                        if let Some(term) = &terminal {
                            if let Some(cmd) = exec.first() {
                                let full_cmd = if exec.len() > 1 {
                                    format!("{} {}", cmd, exec[1..].join(" "))
                                } else {
                                    cmd.to_string()
                                };

                                if let Err(e) = Command::new(term)
                                    .arg("-e")
                                    .arg("sh")
                                    .arg("-c")
                                    .arg(&full_cmd)
                                    .spawn()
                                {
                                    eprintln!("Failed to launch {} in terminal: {}", name, e);
                                    send_notification(
                                        "Launch Error",
                                        &format!("Failed to launch {} in terminal: {}", name, e),
                                    );
                                }
                            }
                        } else {
                            send_notification(
                                "Config Error",
                                "No terminal configured in config.toml",
                            );
                        }
                        app.quit();
                    }),
                )
            },
            {
                let exec = exec_parts.clone();
                let name = app_name.clone();
                let editor = self.config.get_editor().map(|s| s.to_string());
                (
                    DisplayItem {
                        label: format!("Launch {} in editor", app_name),
                        icon: DisplayIcon::named("document-edit"),
                        highlight_indices: None,
                    },
                    MatchHandle::new(move |app: &Application| {
                        if let Some(ed) = &editor {
                            if let Some(cmd) = exec.first() {
                                if let Err(e) = Command::new(ed).arg(cmd).spawn() {
                                    eprintln!("Failed to launch {} in editor: {}", name, e);
                                    send_notification(
                                        "Launch Error",
                                        &format!("Failed to launch {} in editor: {}", name, e),
                                    );
                                }
                            }
                        } else {
                            send_notification(
                                "Config Error",
                                "No editor configured in config.toml",
                            );
                        }
                        app.quit();
                    }),
                )
            },
            {
                let cmd = cmd_string.clone();
                (
                    DisplayItem {
                        label: format!("Copy command: {}", cmd_string),
                        icon: DisplayIcon::named("edit-copy"),
                        highlight_indices: None,
                    },
                    MatchHandle::new(move |app: &Application| {
                        if let Err(e) = Command::new("wl-copy").arg(&cmd).spawn() {
                            eprintln!("Failed to copy command: {}", e);
                            send_notification("Copy Error", "Failed to copy command");
                        } else {
                            send_notification("Action", "Command copied to clipboard");
                        }
                        app.quit();
                    }),
                )
            },
        ]
    }
}
