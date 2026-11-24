use super::{DisplayIcon, DisplayItem, MatchHandle, Plugin};
use crate::config::send_notification;
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use gtk4::prelude::*;
use gtk4::Application;
use gtk4::{gdk, gio, glib};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsString;

#[derive(Clone)]
struct AppEntry {
    name: String,
    name_lower: String,
    app_info: gio::AppInfo,
    icon: DisplayIcon,
}

impl AppEntry {
    fn new_from_appinfo(app: gio::AppInfo) -> Option<Self> {
        if !app.should_show() {
            return None;
        }

        let name = app.display_name().to_string();
        let name_lower = name.to_lowercase();
        let icon = app
            .icon()
            .map(DisplayIcon::GIcon)
            .unwrap_or_else(|| DisplayIcon::named("application-x-executable"));

        Some(Self {
            name,
            name_lower,
            app_info: app,
            icon,
        })
    }

    fn command_parts(&self) -> Vec<OsString> {
        self.app_info
            .commandline()
            .and_then(|command| glib::shell_parse_argv(command).ok())
            .unwrap_or_default()
            .into_iter()
            .filter(|part| !part.to_string_lossy().starts_with('%'))
            .collect()
    }

    fn launch_handle(&self) -> MatchHandle {
        let app_info = self.app_info.clone();
        let name = self.name.clone();
        MatchHandle::new(move |app: &Application| {
            let context = gdk::Display::default().map(|display| display.app_launch_context());
            if let Err(error) = app_info.launch(&[], context.as_ref()) {
                let message = format!("Failed to launch {name}: {error}");
                eprintln!("{message}");
                send_notification("Launch Error", &message);
            }
            app.quit();
        })
    }
}

struct AppCache {
    all: Vec<AppEntry>,
    match_cache: RefCell<HashMap<String, Vec<(i64, usize, Vec<usize>)>>>,
}

impl AppCache {
    fn new() -> Self {
        let mut all: Vec<_> = gio::AppInfo::all()
            .into_iter()
            .filter_map(AppEntry::new_from_appinfo)
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

    pub fn get_app_info(&self, item_index: usize) -> Option<(String, Vec<OsString>, MatchHandle)> {
        let results = self.last_results.borrow();
        let app_idx = *results.get(item_index)?;
        let entry = self.cache.get(app_idx)?;
        Some((
            entry.name.clone(),
            entry.command_parts(),
            entry.launch_handle(),
        ))
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

                (
                    DisplayItem {
                        label: entry.name.clone(),
                        icon: entry.icon.clone(),
                        highlight_indices: indices,
                    },
                    entry.launch_handle(),
                )
            })
            .collect()
    }
}
