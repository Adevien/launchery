use gtk4::glib::{self, Propagation};
use gtk4::{gio, prelude::*, EventControllerFocus};
use gtk4::{
    Align, Application, ApplicationWindow, Box as GtkBox, Button, EventControllerKey,
    EventControllerScroll, Image, Label, Orientation, SearchEntry,
};
use gtk4_layer_shell::{Edge, Layer, LayerShell};
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::Command;
use std::rc::Rc;

use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use serde::Deserialize;
use std::env;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Deserialize, Clone)]
struct HighlightConfig {
    color: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
struct StyleConfig {
    border_radius: Option<i32>,
    border_width: Option<i32>,
}

#[derive(Debug, Deserialize, Clone)]
struct WindowConfig {
    width: Option<i32>,
    max_rows: Option<usize>,
}

#[derive(Debug, Deserialize, Clone)]
struct Config {
    highlight: HighlightConfig,
    style: Option<StyleConfig>,
    window: Option<WindowConfig>,
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
            }),
            window: Some(WindowConfig {
                width: Some(540),
                max_rows: Some(5),
            }),
        }
    }
}

impl Config {
    fn load() -> Self {
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
                send_notification(
                    "Launchery Config",
                    &format!("Could not read config from {:?}: {}", path, e),
                );
                return Self::default();
            }
        };

        toml::from_str(&data).unwrap_or_else(|e| {
            send_notification(
                "Launchery Config",
                &format!("Could not parse config: {}", e),
            );
            Self::default()
        })
    }
}

fn send_notification(summary: &str, body: &str) {
    let _ = Command::new("notify-send").arg(summary).arg(body).spawn();
}

struct Navigator {
    offset: usize,
    selected: usize,
    max_rows: usize,
    len: usize,
}

impl Navigator {
    fn new(max_rows: usize) -> Self {
        Self {
            offset: 0,
            selected: 0,
            max_rows,
            len: 0,
        }
    }

    fn set_len(&mut self, len: usize) {
        self.len = len;
        self.offset = 0;
        self.selected = 0;
    }

    fn move_selection(&mut self, direction: i32) {
        if self.len == 0 {
            return;
        }

        if self.len <= self.max_rows {
            self.selected = (self.selected as isize + direction as isize)
                .rem_euclid(self.len as isize) as usize;
        } else {
            match direction {
                d if d > 0 && self.selected < self.max_rows - 1 => self.selected += 1,
                d if d > 0 => self.offset = (self.offset + 1) % self.len,
                d if d < 0 && self.selected > 0 => self.selected -= 1,
                d if d < 0 => self.offset = (self.offset + self.len - 1) % self.len,
                _ => {}
            }
        }
    }

    fn jump_to_start(&mut self) {
        if self.len == 0 {
            return;
        }
        self.offset = 0;
        self.selected = 0;
    }

    fn jump_to_end(&mut self) {
        if self.len == 0 {
            return;
        }
        if self.len <= self.max_rows {
            self.selected = self.len - 1;
        } else {
            self.offset = self.len - self.max_rows;
            self.selected = self.max_rows - 1;
        }
    }

    fn current_index(&self) -> usize {
        (self.offset + self.selected) % self.len
    }
}

#[derive(Clone)]
struct AppEntry {
    name: String,
    name_lower: String,
    exec_parts: Vec<String>,
    icon: Option<String>,
}

impl AppEntry {
    fn new_from_appinfo(app: gio::AppInfo) -> Option<Self> {
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

        let icon = app.icon().and_then(|icon| {
            if let Some(themed) = icon.downcast_ref::<gio::ThemedIcon>() {
                themed.names().first().map(|s| s.to_string())
            } else if let Some(file_icon) = icon.downcast_ref::<gio::FileIcon>() {
                file_icon
                    .file()
                    .path()
                    .map(|p| p.to_string_lossy().to_string())
            } else {
                None
            }
        });

        Some(Self {
            name,
            name_lower,
            exec_parts,
            icon,
        })
    }
}

struct AppCache {
    all: Vec<AppEntry>,
    match_cache: RefCell<HashMap<(usize, String), Option<(i64, Vec<usize>)>>>,
}

#[derive(Clone)]
struct MatchResult {
    app_index: usize,
    indices: Option<Vec<usize>>,
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

    fn get_entry(&self, index: usize) -> &AppEntry {
        &self.all[index]
    }

    fn filter(
        &self,
        matcher: &SkimMatcherV2,
        query: &str,
        prev_matches: Option<&[MatchResult]>,
    ) -> Vec<MatchResult> {
        if query.is_empty() {
            return (0..self.all.len())
                .map(|app_index| MatchResult {
                    app_index,
                    indices: None,
                })
                .collect();
        }

        let source: Vec<usize> = if let Some(prev) = prev_matches {
            prev.iter().map(|m| m.app_index).collect()
        } else {
            (0..self.all.len()).collect()
        };

        let mut cache = self.match_cache.borrow_mut();
        let mut scored: Vec<_> = source
            .iter()
            .filter_map(|&app_index| {
                let cache_key = (app_index, query.to_string());

                let result = cache.entry(cache_key).or_insert_with(|| {
                    matcher
                        .fuzzy_indices(&self.all[app_index].name_lower, query)
                        .map(|(score, indices)| (score, indices))
                });

                result
                    .as_ref()
                    .map(|(score, indices)| (*score, app_index, indices.clone()))
            })
            .collect();

        scored.sort_unstable_by(|a, b| b.0.cmp(&a.0));
        scored
            .into_iter()
            .map(|(_, app_index, indices)| MatchResult {
                app_index,
                indices: Some(indices),
            })
            .collect()
    }
}

struct IconCache {
    cache: RefCell<HashMap<String, String>>,
}

impl IconCache {
    fn new() -> Self {
        Self {
            cache: RefCell::new(HashMap::new()),
        }
    }

    fn get_or_cache(&self, icon_name: &str) -> String {
        let mut cache = self.cache.borrow_mut();

        if let Some(cached) = cache.get(icon_name) {
            return cached.clone();
        }

        cache.insert(icon_name.to_string(), icon_name.to_string());
        icon_name.to_string()
    }
}

struct AppButton {
    button: Button,
    img: Image,
    label: Label,
    last_app_index: Rc<RefCell<Option<usize>>>,
    last_indices: RefCell<Option<Vec<usize>>>,
    markup_buffer: RefCell<String>,
    config: Rc<Config>,
    icon_cache: Rc<IconCache>,
}

impl AppButton {
    fn new(
        app: &Application,
        config: Rc<Config>,
        icon_cache: Rc<IconCache>,
        cache: Rc<AppCache>,
    ) -> Self {
        let button = Button::new();

        let hbox = GtkBox::new(Orientation::Horizontal, 8);

        let img = Image::from_icon_name("application-x-executable");
        img.set_pixel_size(24);

        let label = Label::new(None);
        label.set_use_markup(true);
        label.set_halign(gtk4::Align::Start);

        hbox.append(&img);
        hbox.append(&label);
        button.set_child(Some(&hbox));

        let last_app_index_for_click = Rc::new(RefCell::new(None));
        let last_app_index_clone = last_app_index_for_click.clone();
        let app = app.clone();
        let cache_clone = cache.clone();

        button.connect_clicked(move |_| {
            if let Some(app_index) = *last_app_index_clone.borrow() {
                let entry = cache_clone.get_entry(app_index);
                if let Some(cmd) = entry.exec_parts.first() {
                    if let Err(e) = Command::new(cmd).args(&entry.exec_parts[1..]).spawn() {
                        send_notification(
                            "Launch Error",
                            &format!("Failed to launch {}: {}", entry.name, e),
                        );
                    }
                }
            }
            app.quit();
        });

        Self {
            button,
            img,
            label,
            last_app_index: last_app_index_for_click,
            last_indices: RefCell::new(None),
            markup_buffer: RefCell::new(String::with_capacity(256)),
            config,
            icon_cache,
        }
    }

    fn update(&self, cache: &AppCache, match_result: &MatchResult, selected: bool) {
        let entry = cache.get_entry(match_result.app_index);
        let needs_update = *self.last_app_index.borrow() != Some(match_result.app_index)
            || *self.last_indices.borrow() != match_result.indices;

        if needs_update {
            if let Some(indices) = &match_result.indices {
                let mut markup = self.markup_buffer.borrow_mut();
                markup.clear();

                let name_chars: Vec<char> = entry.name.chars().collect();

                for (i, c) in name_chars.iter().enumerate() {
                    if indices.contains(&i) {
                        let color = self.config.highlight.color.as_deref().unwrap_or("#64a0ff");
                        markup.push_str(&format!("<span foreground=\"{}\">", color));

                        if c.is_ascii() && *c != '<' && *c != '>' && *c != '&' {
                            markup.push(*c);
                        } else {
                            markup.push_str(&glib::markup_escape_text(&c.to_string()));
                        }
                        markup.push_str("</span>");
                    } else {
                        if c.is_ascii() && *c != '<' && *c != '>' && *c != '&' {
                            markup.push(*c);
                        } else {
                            markup.push_str(&glib::markup_escape_text(&c.to_string()));
                        }
                    }
                }

                self.label.set_markup(&markup);
            } else {
                self.label.set_text(&entry.name);
            }

            *self.last_indices.borrow_mut() = match_result.indices.clone();

            let icon_name = entry.icon.as_deref().unwrap_or("application-x-executable");
            let cached_name = self.icon_cache.get_or_cache(icon_name);
            self.img.set_icon_name(Some(&cached_name));

            *self.last_app_index.borrow_mut() = Some(match_result.app_index);
        }

        if selected {
            self.button
                .set_state_flags(gtk4::StateFlags::CHECKED, false);
        } else {
            self.button.unset_state_flags(gtk4::StateFlags::CHECKED);
        }
    }
}

struct AppState {
    cache: Rc<AppCache>,
    matcher: Rc<SkimMatcherV2>,
    navigator: Rc<RefCell<Navigator>>,
    matches_store: Rc<RefCell<Vec<MatchResult>>>,
    last_query: Rc<RefCell<String>>,
    button_pool: Rc<Vec<AppButton>>,
    last_visible_count: Rc<RefCell<usize>>,
}

fn create_move_selection(
    navigator: Rc<RefCell<Navigator>>,
    redraw_fn: Rc<dyn Fn()>,
) -> Rc<dyn Fn(i32)> {
    Rc::new(move |direction: i32| {
        if direction.abs() > 1000 {
            if direction < 0 {
                navigator.borrow_mut().jump_to_start();
            } else {
                navigator.borrow_mut().jump_to_end();
            }
        } else {
            navigator.borrow_mut().move_selection(direction);
        }
        redraw_fn();
    })
}

fn setup_window(app: &Application, width: i32) -> ApplicationWindow {
    let window = ApplicationWindow::builder()
        .application(app)
        .title("Launcher")
        .resizable(false)
        .decorated(true)
        .halign(Align::Center)
        .valign(Align::Center)
        .modal(true)
        .build();

    window.set_width_request(width);
    window.set_default_size(width, 200);

    window.init_layer_shell();
    window.set_layer(Layer::Overlay);
    window.set_keyboard_mode(gtk4_layer_shell::KeyboardMode::OnDemand);

    window.set_anchor(Edge::Top, true);
    window.set_anchor(Edge::Bottom, false);
    window.set_anchor(Edge::Left, false);
    window.set_anchor(Edge::Right, false);

    window.connect_realize(|win| {
        if let Some(surface) = win.surface() {
            let display = surface.display();
            if let Some(monitor) = display.monitor_at_surface(&surface) {
                let geometry = monitor.geometry();
                let margin = (geometry.height() as f32 * 0.25) as i32;
                win.set_margin(Edge::Top, margin);
            }
        }
    });

    window
}

fn setup_ui_layout(window: &ApplicationWindow) -> (GtkBox, SearchEntry, GtkBox, Label) {
    let vbox = GtkBox::new(Orientation::Vertical, 8);
    vbox.set_margin_top(12);
    vbox.set_margin_bottom(12);
    vbox.set_margin_start(12);
    vbox.set_margin_end(12);

    let search = SearchEntry::new();
    let results_box = GtkBox::new(Orientation::Vertical, 6);
    let count_label = Label::new(Some("0 results"));
    count_label.set_halign(Align::End);

    vbox.append(&search);
    vbox.append(&results_box);
    vbox.append(&count_label);
    window.set_child(Some(&vbox));

    (vbox, search, results_box, count_label)
}

fn setup_button_pool(
    max_rows: usize,
    app: &Application,
    config: Rc<Config>,
    icon_cache: Rc<IconCache>,
    cache: Rc<AppCache>,
    results_box: &GtkBox,
) -> Rc<Vec<AppButton>> {
    let button_pool: Rc<Vec<AppButton>> = Rc::new(
        (0..max_rows)
            .map(|_| AppButton::new(app, config.clone(), icon_cache.clone(), cache.clone()))
            .collect(),
    );

    for btn in button_pool.iter() {
        btn.button.set_visible(false);
        results_box.append(&btn.button);
    }

    button_pool
}

fn create_redraw_closure(
    state: &AppState,
    vbox: &GtkBox,
    window: &ApplicationWindow,
    count_label: &Label,
    max_rows: usize,
    fixed_width: i32,
) -> Rc<dyn Fn()> {
    let matches_store = state.matches_store.clone();
    let navigator = state.navigator.clone();
    let button_pool = state.button_pool.clone();
    let vbox_clone = vbox.clone();
    let win_clone = window.clone();
    let count_label_clone = count_label.clone();
    let last_visible_count = state.last_visible_count.clone();
    let cache = state.cache.clone();

    Rc::new(move || {
        let matches = matches_store.borrow();
        let nav = navigator.borrow();
        let len = matches.len();

        count_label_clone.set_text(&format!(
            "{} result{}",
            len,
            if len == 1 { "" } else { "s" }
        ));

        let offset = nav.offset;
        let selected = nav.selected;
        let visible = max_rows.min(len);

        for (i, btn) in button_pool.iter().enumerate() {
            if i < visible {
                let idx = (offset + i) % len;
                btn.update(&cache, &matches[idx], i == selected);
                btn.button.set_visible(true);
            } else {
                btn.button.set_visible(false);
            }
        }

        if visible < max_rows && *last_visible_count.borrow() != visible {
            *last_visible_count.borrow_mut() = visible;
            vbox_clone.queue_resize();
            glib::idle_add_local_once({
                let win_clone = win_clone.clone();
                let vbox_clone = vbox_clone.clone();
                move || {
                    let (_, natural_height, _, _) =
                        vbox_clone.measure(Orientation::Vertical, fixed_width);
                    win_clone.set_default_size(fixed_width, natural_height);
                }
            });
        }
    })
}

fn create_launch_closure(state: &AppState, app: &Application) -> Rc<dyn Fn()> {
    let matches_store = state.matches_store.clone();
    let navigator = state.navigator.clone();
    let app_clone = app.clone();
    let cache = state.cache.clone();

    Rc::new(move || {
        let matches = matches_store.borrow();
        let nav = navigator.borrow();
        if nav.len == 0 {
            return;
        }
        let idx = nav.current_index();
        let entry = cache.get_entry(matches[idx].app_index);
        if let Some(cmd) = entry.exec_parts.first() {
            if let Err(e) = Command::new(cmd).args(&entry.exec_parts[1..]).spawn() {
                send_notification(
                    "Launch Error",
                    &format!("Failed to launch {}: {}", entry.name, e),
                );
            }
        }
        app_clone.quit();
    })
}

fn setup_search_handler(
    search: &SearchEntry,
    state: &AppState,
    launch_fn: Rc<dyn Fn()>,
    redraw_fn: Rc<dyn Fn()>,
) {
    let pending_query: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
    let update_scheduled: Rc<RefCell<bool>> = Rc::new(RefCell::new(false));

    let cache = state.cache.clone();
    let matcher = state.matcher.clone();
    let matches_store = state.matches_store.clone();
    let navigator = state.navigator.clone();
    let last_query = state.last_query.clone();

    let do_update = {
        let update_scheduled = update_scheduled.clone();
        let redraw_fn = redraw_fn.clone();

        Rc::new(move |query: String| {
            let query_lc = query.to_lowercase();

            let matches = if query_lc.starts_with(&*last_query.borrow())
                && !matches_store.borrow().is_empty()
            {
                let current = matches_store.borrow();
                cache.filter(&matcher, &query_lc, Some(&current))
            } else {
                cache.filter(&matcher, &query_lc, None)
            };

            *last_query.borrow_mut() = query_lc;

            let len = matches.len();
            navigator.borrow_mut().set_len(len);
            *matches_store.borrow_mut() = matches;

            redraw_fn();
            *update_scheduled.borrow_mut() = false;
        })
    };

    {
        let pending_query = pending_query.clone();
        let update_scheduled = update_scheduled.clone();
        let do_update = do_update.clone();

        search.connect_changed(move |entry| {
            let query = entry.text().to_string();
            *pending_query.borrow_mut() = Some(query.clone());

            if !*update_scheduled.borrow() {
                *update_scheduled.borrow_mut() = true;
                let pending_query = pending_query.clone();
                let do_update = do_update.clone();

                glib::idle_add_local_once(move || {
                    if let Some(query) = pending_query.borrow_mut().take() {
                        do_update(query);
                    }
                });
            }
        });
    }

    search.connect_activate(move |_| {
        launch_fn();
    });
}

fn setup_keybindings(search: &SearchEntry, move_fn: Rc<dyn Fn(i32)>, app: &Application) {
    let app_for_escape = app.clone();

    let key_ctrl = EventControllerKey::new();

    key_ctrl.connect_key_pressed(move |_, keyval, _, _state| {
        let key_name = keyval.name();
        let key_str = key_name.as_deref();

        match key_str {
            Some("Up") => {
                move_fn(-1);
                Propagation::Stop
            }
            Some("Down") | Some("Tab") => {
                move_fn(1);
                Propagation::Stop
            }
            Some("Home") => {
                move_fn(-999999);
                Propagation::Stop
            }
            Some("End") => {
                move_fn(999999);
                Propagation::Stop
            }
            Some("Escape") => {
                app_for_escape.quit();
                Propagation::Stop
            }
            _ => Propagation::Proceed,
        }
    });

    search.add_controller(key_ctrl);
    search.set_focusable(true);
}

fn setup_scroll_handler(
    results_box: &GtkBox,
    move_fn: Rc<dyn Fn(i32)>,
    state: &AppState,
    max_rows: usize,
) {
    let navigator_scroll = state.navigator.clone();

    let scroll_ctrl = EventControllerScroll::new(gtk4::EventControllerScrollFlags::VERTICAL);

    scroll_ctrl.connect_scroll(move |_, _, dy| {
        if navigator_scroll.borrow().len > max_rows {
            if dy > 0.0 {
                move_fn(1);
            } else if dy < 0.0 {
                move_fn(-1);
            }
            Propagation::Stop
        } else {
            Propagation::Proceed
        }
    });
    results_box.add_controller(scroll_ctrl);
}

fn setup_focus_handler(window: &ApplicationWindow, app: &Application) {
    let app_clone = app.clone();
    let controller = EventControllerFocus::new();
    controller.connect_leave(move |_| {
        app_clone.quit();
    });
    window.add_controller(controller);
}

fn apply_styling(window: &ApplicationWindow, config: &Config) {
    let border_radius = config
        .style
        .as_ref()
        .and_then(|s| s.border_radius)
        .unwrap_or(0);

    let border_width = config
        .style
        .as_ref()
        .and_then(|s| s.border_width)
        .unwrap_or(0);

    let css_provider = gtk4::CssProvider::new();
    css_provider.load_from_data(&format!(
        "window {{ border-radius: {}px; border: {}px solid @accent_bg_color; }}",
        border_radius, border_width
    ));

    let display = gtk4::prelude::WidgetExt::display(window);
    gtk4::style_context_add_provider_for_display(
        &display,
        &css_provider,
        gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION,
    );
}

fn play_fade_animation(vbox: &GtkBox, window: &ApplicationWindow, search: &SearchEntry) {
    vbox.set_opacity(0.0);
    window.present();

    let target = adw::CallbackAnimationTarget::new({
        let vbox = vbox.clone();
        move |value| {
            vbox.set_opacity(value);
        }
    });

    let animation = adw::TimedAnimation::new(vbox, 0.0, 1.0, 300, target);
    adw::prelude::AnimationExt::play(&animation);

    search.grab_focus();
}

fn build_app_window(app: &Application, config: Rc<Config>) {
    let width = config.window.as_ref().and_then(|w| w.width).unwrap_or(540);

    let max_rows = config.window.as_ref().and_then(|w| w.max_rows).unwrap_or(5);

    let window = setup_window(app, width);
    let (vbox, search, results_box, count_label) = setup_ui_layout(&window);

    let cache = Rc::new(AppCache::new());
    let icon_cache = Rc::new(IconCache::new());
    let matcher = Rc::new(SkimMatcherV2::default());
    let navigator = Rc::new(RefCell::new(Navigator::new(max_rows)));
    let matches_store: Rc<RefCell<Vec<MatchResult>>> = Rc::new(RefCell::new(Vec::new()));
    let last_query = Rc::new(RefCell::new(String::new()));

    let button_pool = setup_button_pool(
        max_rows,
        app,
        config.clone(),
        icon_cache.clone(),
        cache.clone(),
        &results_box,
    );

    let last_visible_count = Rc::new(RefCell::new(0));

    let state = AppState {
        cache: cache.clone(),
        matcher,
        navigator: navigator.clone(),
        matches_store: matches_store.clone(),
        last_query: last_query.clone(),
        button_pool,
        last_visible_count,
    };

    let redraw_page = create_redraw_closure(&state, &vbox, &window, &count_label, max_rows, width);
    let launch_selected = create_launch_closure(&state, app);

    let move_selection = create_move_selection(navigator.clone(), redraw_page.clone());

    setup_search_handler(
        &search,
        &state,
        launch_selected.clone(),
        redraw_page.clone(),
    );
    setup_keybindings(&search, move_selection.clone(), app);
    setup_scroll_handler(&results_box, move_selection.clone(), &state, max_rows);
    setup_focus_handler(&window, app);
    apply_styling(&window, &config);

    {
        let cache = cache.clone();
        let matcher = state.matcher.clone();
        let matches_store = state.matches_store.clone();
        let navigator = state.navigator.clone();
        let redraw_page = redraw_page.clone();

        let matches = cache.filter(&matcher, "", None);
        let len = matches.len();
        navigator.borrow_mut().set_len(len);
        *matches_store.borrow_mut() = matches;
        redraw_page();
    }

    play_fade_animation(&vbox, &window, &search);
}

fn main() {
    adw::init().expect("error adw could not load");
    let config = Rc::new(Config::load());

    let app = Application::builder()
        .application_id("com.example.Launchery")
        .flags(gio::ApplicationFlags::HANDLES_COMMAND_LINE)
        .build();

    std::env::set_var("GDK_BACKEND", "wayland");
    std::env::set_var("GSK_RENDERER", "cairo");
    let window_handle: Rc<RefCell<Option<ApplicationWindow>>> = Rc::new(RefCell::new(None));

    {
        let window_handle = window_handle.clone();
        let config = config.clone();
        app.connect_activate(move |app| {
            if window_handle.borrow().is_some() {
                return;
            }

            build_app_window(app, config.clone());
        });
    }

    {
        let window_handle = window_handle.clone();
        app.connect_command_line(move |app, _cmd| {
            if let Some(win) = window_handle.borrow_mut().take() {
                if win.is_visible() {
                    app.quit();
                    return 0;
                }
            }
            app.activate();
            0
        });
    }

    app.run();
}
