use serde::Deserialize;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug, Deserialize, Clone)]
pub struct HighlightConfig {
    pub color: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct StyleConfig {
    pub border_radius: Option<i32>,
    pub border_width: Option<i32>,
    pub border_color: Option<String>,
    pub text_color: Option<String>,
    pub font: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct WindowConfig {
    pub width: Option<i32>,
    pub max_rows: Option<usize>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ApplicationConfig {
    pub terminal: Option<String>,
    pub editor: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ScriptsConfig {
    pub allowed_shebangs: Option<Vec<String>>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Config {
    pub highlight: HighlightConfig,
    pub style: Option<StyleConfig>,
    pub window: Option<WindowConfig>,
    pub application: Option<ApplicationConfig>,
    pub scripts: Option<ScriptsConfig>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            highlight: HighlightConfig {
                color: Some("#64a0ff".into()),
            },
            style: Some(StyleConfig {
                border_radius: Some(12),
                border_width: Some(2),
                border_color: Some("@accent_bg_color".into()),
                text_color: None,
                font: None,
            }),
            window: Some(WindowConfig {
                width: Some(540),
                max_rows: Some(5),
            }),
            application: None,
            scripts: Some(ScriptsConfig {
                allowed_shebangs: Some(vec!["#!/bin/bash".into(), "#!/usr/bin/env bash".into()]),
            }),
        }
    }
}

impl Config {
    pub fn load() -> Self {
        let path: PathBuf = if cfg!(debug_assertions) {
            PathBuf::from("config.toml")
        } else {
            let mut base = dirs::config_dir().unwrap_or_else(|| {
                let home = env::var("HOME").expect("Could not get HOME env var");
                PathBuf::from(home).join(".config")
            });
            base.push("launchery");
            base.push("config.toml");
            base
        };

        let data = match fs::read_to_string(&path) {
            Ok(data) => data,
            Err(e) => {
                eprintln!("Could not read config from {:?}: {}", path, e);
                send_notification(
                    "Launchery Config",
                    &format!("Could not read config from {:?}: {}", path, e),
                );
                return Self::default();
            }
        };

        toml::from_str(&data).unwrap_or_else(|e| {
            eprintln!("Could not parse config: {}", e);
            send_notification(
                "Launchery Config",
                &format!("Could not parse config: {}", e),
            );
            Self::default()
        })
    }

    pub fn get_terminal(&self) -> Option<&str> {
        self.application
            .as_ref()
            .and_then(|a| a.terminal.as_deref())
    }

    pub fn get_editor(&self) -> Option<&str> {
        self.application.as_ref().and_then(|a| a.editor.as_deref())
    }

    pub fn get_allowed_script_shebangs(&self) -> Vec<String> {
        self.scripts
            .as_ref()
            .and_then(|s| s.allowed_shebangs.clone())
            .filter(|shebangs| !shebangs.is_empty())
            .unwrap_or_else(|| vec!["#!/bin/bash".into(), "#!/usr/bin/env bash".into()])
    }
}

pub fn send_notification(summary: &str, body: &str) {
    if let Err(e) = Command::new("notify-send").arg(summary).arg(body).spawn() {
        eprintln!("Failed to send notification: {}", e);
    }
}
