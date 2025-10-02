use gtk4::glib::{self, Propagation};
use gtk4::{gio, prelude::*, EventControllerFocus};
use gtk4::{
    Align, Application, ApplicationWindow, Box as GtkBox, Button, EventControllerKey,
    EventControllerScroll, Image, Label, Orientation, SearchEntry,
};
use gtk4_layer_shell::{Edge, Layer, LayerShell};
use std::cell::RefCell;
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
struct Config {
    highlight: HighlightConfig,
    style: Option<StyleConfig>,
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

        let data = fs::read_to_string(&path).unwrap_or_else(|e| {
            eprintln!("Could not read config from {:?}: {}", path, e);
            String::new()
        });

        toml::from_str(&data).unwrap_or_else(|e| {
            eprintln!("Could not parse config: {}", e);
            Config {
                highlight: HighlightConfig {
                    color: Some("#64a0ff".into()),
                },
                style: Some(StyleConfig {
                    border_radius: Some(12),
                    border_width: Some(2),
                }),
            }
        })
    }
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
}

#[derive(Clone)]
struct MatchResult {
    entry: AppEntry,
    indices: Option<Vec<usize>>,
}

impl AppCache {
    fn new() -> Self {
        let mut all: Vec<_> = gio::AppInfo::all()
            .into_iter()
            .filter_map(AppEntry::new_from_appinfo)
            .collect();

        all.sort_by(|a, b| a.name_lower.cmp(&b.name_lower));
        Self { all }
    }

    fn filter(
        &self,
        matcher: &SkimMatcherV2,
        query: &str,
        prev_matches: Option<&[MatchResult]>,
    ) -> Vec<MatchResult> {
        if query.is_empty() {
            return self
                .all
                .iter()
                .map(|entry| MatchResult {
                    entry: entry.clone(),
                    indices: None,
                })
                .collect();
        }

        let source: Vec<&AppEntry> = if let Some(prev) = prev_matches {
            prev.iter().map(|m| &m.entry).collect()
        } else {
            self.all.iter().collect()
        };

        let mut scored: Vec<_> = source
            .iter()
            .filter_map(|app| {
                matcher
                    .fuzzy_indices(&app.name_lower, query)
                    .map(|(score, indices)| (score, (*app).clone(), indices))
            })
            .collect();

        scored.sort_unstable_by(|a, b| b.0.cmp(&a.0));
        scored
            .into_iter()
            .map(|(_, entry, indices)| MatchResult {
                entry,
                indices: Some(indices),
            })
            .collect()
    }
}

struct AppButton {
    button: Button,
    img: Image,
    label: Label,
    exec_parts: Rc<RefCell<Vec<String>>>,
    last_name: RefCell<String>,
    last_indices: RefCell<Option<Vec<usize>>>,
    last_icon: RefCell<Option<String>>,
    markup_buffer: RefCell<String>,
    config: Rc<Config>,
}

impl AppButton {
    fn new(app: &Application, window: &ApplicationWindow, config: Rc<Config>) -> Self {
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

        let exec_parts = Rc::new(RefCell::new(Vec::new()));
        let exec_parts_clone = exec_parts.clone();
        let window = window.clone();
        let app = app.clone();

        button.connect_clicked(move |_| {
            let parts = exec_parts_clone.borrow();
            if let Some(cmd) = parts.first() {
                let _ = Command::new(cmd).args(&parts[1..]).spawn();
            }
            window.close();
            app.quit();
        });

        Self {
            button,
            img,
            label,
            exec_parts,
            last_name: RefCell::new(String::new()),
            last_indices: RefCell::new(None),
            last_icon: RefCell::new(None),
            markup_buffer: RefCell::new(String::with_capacity(256)),
            config,
        }
    }

    fn update(&self, match_result: &MatchResult, selected: bool) {
        let entry = &match_result.entry;
        let needs_update = *self.last_name.borrow() != entry.name
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

            *self.last_name.borrow_mut() = entry.name.clone();
            *self.last_indices.borrow_mut() = match_result.indices.clone();
        }

        if *self.last_icon.borrow() != entry.icon {
            let icon_name = entry.icon.as_deref().unwrap_or("application-x-executable");
            self.img.set_icon_name(Some(icon_name));
            *self.last_icon.borrow_mut() = entry.icon.clone();
        }

        if needs_update {
            *self.exec_parts.borrow_mut() = entry.exec_parts.clone();
        }

        if selected {
            self.button
                .set_state_flags(gtk4::StateFlags::CHECKED, false);
        } else {
            self.button.unset_state_flags(gtk4::StateFlags::CHECKED);
        }
    }
}

fn main() {
    adw::init().expect("error adw could not load");
    let config = Rc::new(Config::load());

    let app = Application::builder()
        .application_id("com.adv.Launchery")
        .flags(gio::ApplicationFlags::HANDLES_COMMAND_LINE)
        .build();

    std::env::set_var("GDK_BACKEND", "wayland");

    let window_handle: Rc<RefCell<Option<ApplicationWindow>>> = Rc::new(RefCell::new(None));

    {
        let window_handle = window_handle.clone();
        app.connect_activate(move |app| {
            if window_handle.borrow().is_some() {
                return;
            }

            const FIXED_WIDTH: i32 = 540;
            const MAX_ROWS: usize = 5;

            let window = ApplicationWindow::builder()
                .application(app)
                .title("Launcher")
                .resizable(false)
                .decorated(true)
                .halign(Align::Center)
                .valign(Align::Center)
                .modal(true)
                .build();

            window.set_width_request(FIXED_WIDTH);
            window.set_default_size(FIXED_WIDTH, 200);

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

            let cache = Rc::new(AppCache::new());
            let matcher = Rc::new(SkimMatcherV2::default());
            let navigator = Rc::new(RefCell::new(Navigator::new(MAX_ROWS)));
            let matches_store: Rc<RefCell<Vec<MatchResult>>> = Rc::new(RefCell::new(Vec::new()));
            let last_query = Rc::new(RefCell::new(String::new()));

            let button_pool: Rc<Vec<AppButton>> = Rc::new(
                (0..MAX_ROWS)
                    .map(|_| AppButton::new(&app, &window, config.clone()))
                    .collect(),
            );

            for btn in button_pool.iter() {
                btn.button.set_visible(false);
                results_box.append(&btn.button);
            }

            let last_visible_count = Rc::new(RefCell::new(0));

            let redraw_page = {
                let matches_store = matches_store.clone();
                let navigator = navigator.clone();
                let button_pool = button_pool.clone();
                let vbox_clone = vbox.clone();
                let win_clone = window.clone();
                let count_label_clone = count_label.clone();
                let last_visible_count = last_visible_count.clone();

                move || {
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
                    let visible = MAX_ROWS.min(len);

                    for (i, btn) in button_pool.iter().enumerate() {
                        if i < visible {
                            let idx = (offset + i) % len;
                            btn.update(&matches[idx], i == selected);
                            btn.button.set_visible(true);
                        } else {
                            btn.button.set_visible(false);
                        }
                    }

                    if visible < MAX_ROWS && *last_visible_count.borrow() != visible {
                        *last_visible_count.borrow_mut() = visible;
                        vbox_clone.queue_resize();
                        glib::idle_add_local_once({
                            let win_clone = win_clone.clone();
                            let vbox_clone = vbox_clone.clone();
                            move || {
                                let (_, natural_height, _, _) =
                                    vbox_clone.measure(Orientation::Vertical, FIXED_WIDTH);
                                win_clone.set_default_size(FIXED_WIDTH, natural_height);
                            }
                        });
                    }
                }
            };

            let move_selection = {
                let navigator = navigator.clone();
                let redraw_page = redraw_page.clone();

                move |direction: i32| {
                    navigator.borrow_mut().move_selection(direction);
                    redraw_page();
                }
            };

            let launch_selected = {
                let matches_store = matches_store.clone();
                let navigator = navigator.clone();
                let app_clone = app.clone();

                move || {
                    let matches = matches_store.borrow();
                    let nav = navigator.borrow();
                    if nav.len == 0 {
                        return;
                    }
                    let idx = nav.current_index();
                    let entry = &matches[idx].entry;
                    if let Some(cmd) = entry.exec_parts.first() {
                        let _ = Command::new(cmd).args(&entry.exec_parts[1..]).spawn();
                    }
                    app_clone.quit();
                }
            };

            let pending_query: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
            let update_scheduled: Rc<RefCell<bool>> = Rc::new(RefCell::new(false));

            let do_update = {
                let cache = cache.clone();
                let matcher = matcher.clone();
                let matches_store = matches_store.clone();
                let navigator = navigator.clone();
                let redraw_page = redraw_page.clone();
                let last_query = last_query.clone();
                let update_scheduled = update_scheduled.clone();

                move |query: String| {
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

                    redraw_page();
                    *update_scheduled.borrow_mut() = false;
                }
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

                let launch_selected = launch_selected.clone();
                search.connect_activate(move |_| {
                    launch_selected();
                });

                let move_selection_tab = move_selection.clone();
                let move_selection_arrows = move_selection.clone();
                let app_for_escape = app.clone();

                let key_ctrl = EventControllerKey::new();
                key_ctrl.connect_key_pressed(move |_, keyval, _, _| {
                    match keyval.name().as_deref() {
                        Some("Tab") => {
                            move_selection_tab(1);
                            Propagation::Stop
                        }
                        Some("Up") => {
                            move_selection_arrows(-1);
                            Propagation::Stop
                        }
                        Some("Down") => {
                            move_selection_arrows(1);
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

            {
                let move_selection_scroll = move_selection.clone();
                let navigator_scroll = navigator.clone();

                let scroll_ctrl =
                    EventControllerScroll::new(gtk4::EventControllerScrollFlags::VERTICAL);

                scroll_ctrl.connect_scroll(move |_, _, dy| {
                    if navigator_scroll.borrow().len > MAX_ROWS {
                        if dy > 0.0 {
                            move_selection_scroll(1);
                        } else if dy < 0.0 {
                            move_selection_scroll(-1);
                        }
                        Propagation::Stop
                    } else {
                        Propagation::Proceed
                    }
                });
                results_box.add_controller(scroll_ctrl);
            }

            do_update(String::new());

            window.add_controller({
                let _win = window.clone();
                let _app = app.clone();
                let controller = EventControllerFocus::new();
                controller.connect_leave(move |_| {
                    _app.quit();
                });
                controller
            });

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

            let display = gtk4::prelude::WidgetExt::display(&window);
            gtk4::style_context_add_provider_for_display(
                &display,
                &css_provider,
                gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );

            vbox.set_opacity(0.0);

            window.present();

            let target = adw::CallbackAnimationTarget::new({
                let vbox = vbox.clone();
                move |value| {
                    vbox.set_opacity(value);
                }
            });

            let animation = adw::TimedAnimation::new(&vbox, 0.0, 1.0, 300, target);
            adw::prelude::AnimationExt::play(&animation);

            search.grab_focus();
            *window_handle.borrow_mut() = Some(window);
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
