use super::{DisplayIcon, DisplayItem, MatchHandle, Plugin};
use crate::config::send_notification;
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use gtk4::gio;
use gtk4::prelude::*;
use gtk4::Application;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Clone)]
struct AppEntry {
    name: String,
    name_lower: String,
    exec_parts: Vec<String>,
    icon: DisplayIcon,
}

impl AppEntry {
    fn new_from_appinfo(
        app: &gio::AppInfo,
        icon_file_cache: &mut HashMap<String, Option<PathBuf>>,
    ) -> Option<Self> {
        if !app.should_show() {
            return None;
        }

        let name = app.display_name().to_string();
        let name_lower = name.to_lowercase();

        let exec_parts: Vec<String> = app
            .commandline()
            .map(|p| {
                p.to_string_lossy()
                    .split_whitespace()
                    .filter(|s| !s.starts_with('%'))
                    .map(|s| s.to_string())
                    .collect()
            })
            .unwrap_or_default();

        let icon = resolve_app_icon(app, icon_file_cache);

        Some(Self {
            name,
            name_lower,
            exec_parts,
            icon,
        })
    }
}

fn resolve_app_icon(
    app: &gio::AppInfo,
    icon_file_cache: &mut HashMap<String, Option<PathBuf>>,
) -> DisplayIcon {
    if let Some(desktop_app) = app.downcast_ref::<gio::DesktopAppInfo>() {
        if let Some(icon_key) = desktop_app.string("Icon") {
            let icon_key = icon_key.to_string();
            let icon_path = Path::new(&icon_key);

            if icon_path.is_absolute() && icon_path.exists() {
                return DisplayIcon::File(icon_key);
            }

            let resolved = icon_file_cache
                .entry(icon_key.clone())
                .or_insert_with(|| find_icon_file(&icon_key));

            if let Some(resolved) = resolved {
                return DisplayIcon::File(resolved.to_string_lossy().to_string());
            }
        }
    }

    app.icon()
        .map(DisplayIcon::GIcon)
        .unwrap_or_else(|| DisplayIcon::named("application-x-executable"))
}

fn find_icon_file(icon_name: &str) -> Option<PathBuf> {
    let icon_name = icon_name
        .strip_suffix(".png")
        .or_else(|| icon_name.strip_suffix(".svg"))
        .or_else(|| icon_name.strip_suffix(".xpm"))
        .unwrap_or(icon_name);

    icon_search_dirs()
        .into_iter()
        .filter_map(|dir| find_icon_file_in_dir(&dir, icon_name))
        .max_by_key(|path| icon_size_score(path))
}

fn icon_search_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    if let Some(data_home) = std::env::var_os("XDG_DATA_HOME") {
        dirs.push(PathBuf::from(data_home).join("icons"));
    } else if let Some(home) = std::env::var_os("HOME") {
        dirs.push(PathBuf::from(home).join(".local/share/icons"));
    }

    if let Some(home) = std::env::var_os("HOME") {
        dirs.push(PathBuf::from(home).join(".icons"));
    }

    if let Some(data_dirs) = std::env::var_os("XDG_DATA_DIRS") {
        dirs.extend(
            std::env::split_paths(&data_dirs)
                .map(|path| path.join("icons"))
                .collect::<Vec<_>>(),
        );
    } else {
        dirs.push(PathBuf::from("/usr/local/share/icons"));
        dirs.push(PathBuf::from("/usr/share/icons"));
    }

    dirs
}

fn find_icon_file_in_dir(dir: &Path, icon_name: &str) -> Option<PathBuf> {
    let entries = fs::read_dir(dir).ok()?;
    let mut best = None;
    let mut best_score = 0;

    for entry in entries.flatten() {
        let path = entry.path();

        if path.is_dir() {
            if let Some(candidate) = find_icon_file_in_dir(&path, icon_name) {
                let score = icon_size_score(&candidate);
                if score > best_score {
                    best_score = score;
                    best = Some(candidate);
                }
            }
        } else if path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .is_some_and(|stem| stem == icon_name)
            && path
                .extension()
                .and_then(|ext| ext.to_str())
                .is_some_and(|ext| matches!(ext, "png" | "svg" | "xpm"))
        {
            let score = icon_size_score(&path);
            if score > best_score {
                best_score = score;
                best = Some(path);
            }
        }
    }

    best
}

fn icon_size_score(path: &Path) -> u32 {
    path.components()
        .filter_map(|component| component.as_os_str().to_str())
        .filter_map(|part| {
            let size = part.split_once('x')?.0.parse::<u32>().ok()?;
            Some(size)
        })
        .max()
        .unwrap_or(1)
}

struct AppCache {
    all: Vec<AppEntry>,
    match_cache: RefCell<HashMap<String, Vec<(i64, usize, Vec<usize>)>>>,
}

impl AppCache {
    fn new() -> Self {
        let mut icon_file_cache = HashMap::new();
        let mut all: Vec<_> = gio::AppInfo::all()
            .iter()
            .filter_map(|app| AppEntry::new_from_appinfo(app, &mut icon_file_cache))
            .collect();

        all.sort_by(|a, b| a.name_lower.cmp(&b.name_lower));

        Self {
            all,
            match_cache: RefCell::new(HashMap::new()),
        }
    }

    fn search(&self, matcher: &SkimMatcherV2, query: &str) -> Vec<(usize, Option<Vec<usize>>)> {
        if query.is_empty() {
            return (0..self.all.len()).map(|i| (i, None)).collect();
        }

        let query = query.to_lowercase();
        let mut cache = self.match_cache.borrow_mut();

        let results = if let Some(cached) = cache.get(&query) {
            cached.clone()
        } else {
            let mut scored: Vec<_> = self
                .all
                .iter()
                .enumerate()
                .filter_map(|(i, entry)| {
                    matcher
                        .fuzzy_indices(&entry.name_lower, &query)
                        .map(|(score, indices)| (score, i, indices))
                })
                .collect();

            scored.sort_unstable_by(|a, b| b.0.cmp(&a.0));
            cache.insert(query, scored.clone());
            scored
        };

        results
            .into_iter()
            .map(|(_, idx, indices)| (idx, Some(indices)))
            .collect()
    }

    fn get(&self, idx: usize) -> Option<&AppEntry> {
        self.all.get(idx)
    }
}

pub struct AppLauncherPlugin {
    cache: AppCache,
    matcher: SkimMatcherV2,
    last_results: RefCell<Vec<usize>>,
}

impl AppLauncherPlugin {
    pub fn new() -> Self {
        Self {
            cache: AppCache::new(),
            matcher: SkimMatcherV2::default(),
            last_results: RefCell::new(Vec::new()),
        }
    }

    pub fn get_app_info(&self, item_index: usize) -> Option<(String, Vec<String>)> {
        let results = self.last_results.borrow();
        let app_idx = *results.get(item_index)?;
        let entry = self.cache.get(app_idx)?;
        Some((entry.name.clone(), entry.exec_parts.clone()))
    }
}

impl Plugin for AppLauncherPlugin {
    fn name(&self) -> &str {
        "app_launcher"
    }

    fn can_handle(&self, _query: &str) -> bool {
        true
    }

    fn search(&self, query: &str) -> Vec<(DisplayItem, MatchHandle)> {
        let results = self.cache.search(&self.matcher, query);

        *self.last_results.borrow_mut() = results.iter().map(|(idx, _)| *idx).collect();

        results
            .into_iter()
            .map(|(idx, indices)| {
                let entry = &self.cache.all[idx];
                let exec_parts = entry.exec_parts.clone();
                let app_name = entry.name.clone();
                let icon = entry.icon.clone();

                (
                    DisplayItem {
                        label: entry.name.clone(),
                        icon,
                        highlight_indices: indices,
                    },
                    MatchHandle::new(move |app: &Application| {
                        if let Some(cmd) = exec_parts.first() {
                            if let Err(e) = Command::new(cmd).args(&exec_parts[1..]).spawn() {
                                eprintln!("Failed to launch {}: {}", app_name, e);
                                send_notification(
                                    "Launch Error",
                                    &format!("Failed to launch {}: {}", app_name, e),
                                );
                            }
                        }
                        app.quit();
                    }),
                )
            })
            .collect()
    }
}
