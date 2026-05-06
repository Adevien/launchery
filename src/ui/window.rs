use crate::config::Config;
use crate::navigator::Navigator;
use crate::plugins::{
    actions::ActionsPlugin, app_launcher::AppLauncherPlugin, scripts::ScriptsPlugin, DisplayIcon,
    DisplayItem, MatchHandle, Plugin,
};
use gtk4::glib;
use gtk4::prelude::*;
use gtk4::{
    Align, Application, ApplicationWindow, Box as GtkBox, Button, Image, Label, Orientation,
    SearchEntry,
};
use gtk4_layer_shell::{Edge, Layer, LayerShell};
use std::cell::RefCell;
use std::rc::Rc;

struct ResultButton {
    button: Button,
    img: Image,
    label: Label,
    markup_buffer: RefCell<String>,
    config: Rc<Config>,
}

impl ResultButton {
    fn new(config: Rc<Config>) -> Self {
        let button = Button::new();
        let hbox = GtkBox::new(Orientation::Horizontal, 8);

        let img = Image::from_icon_name("application-x-executable");
        img.set_pixel_size(24);

        let label = Label::new(None);
        label.set_use_markup(true);
        label.set_halign(Align::Start);

        hbox.append(&img);
        hbox.append(&label);
        button.set_child(Some(&hbox));

        Self {
            button,
            img,
            label,
            markup_buffer: RefCell::new(String::with_capacity(256)),
            config,
        }
    }

    fn update(&self, item: &DisplayItem, selected: bool) {
        if let Some(indices) = &item.highlight_indices {
            let mut markup = self.markup_buffer.borrow_mut();
            markup.clear();

            let name_chars: Vec<char> = item.label.chars().collect();
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
                } else if c.is_ascii() && *c != '<' && *c != '>' && *c != '&' {
                    markup.push(*c);
                } else {
                    markup.push_str(&glib::markup_escape_text(&c.to_string()));
                }
            }
            self.label.set_markup(&markup);
        } else {
            self.label.set_text(&item.label);
        }

        self.set_icon(&item.icon);

        if selected {
            self.button
                .set_state_flags(gtk4::StateFlags::CHECKED, false);
        } else {
            self.button.unset_state_flags(gtk4::StateFlags::CHECKED);
        }
    }

    fn set_icon(&self, icon: &DisplayIcon) {
        match icon {
            DisplayIcon::Named(name) => self.img.set_icon_name(Some(name)),
            DisplayIcon::File(path) => self.img.set_from_file(Some(path)),
            DisplayIcon::GIcon(icon) => self.img.set_from_gicon(icon),
        }
        self.img.set_pixel_size(24);
    }
}

struct SearchResult {
    item: DisplayItem,
    handle: MatchHandle,
    plugin_idx: usize,
    item_idx: usize,
}

pub fn build_app_window(app: &Application, config: Rc<Config>) {
    let width = config.window.as_ref().and_then(|w| w.width).unwrap_or(540);
    let max_rows = config.window.as_ref().and_then(|w| w.max_rows).unwrap_or(5);

    let window = setup_window(app, width);
    let (vbox, search, results_box, count_label) = setup_ui_layout(&window);

    let app_launcher = Rc::new(AppLauncherPlugin::new());
    let app_launcher_with_actions = ActionsPlugin::new(app_launcher.clone(), config.clone());

    let plugins: Rc<Vec<Box<dyn Plugin>>> = Rc::new(vec![
        Box::new(ScriptsPlugin::new(config.clone())),
        Box::new(app_launcher_with_actions),
    ]);

    let navigator = Rc::new(RefCell::new(Navigator::new(max_rows)));
    let results: Rc<RefCell<Vec<SearchResult>>> = Rc::new(RefCell::new(Vec::new()));
    let in_actions_mode = Rc::new(RefCell::new(false));
    let last_query = Rc::new(RefCell::new(String::new()));

    let button_pool: Vec<ResultButton> = (0..max_rows.max(6))
        .map(|_| ResultButton::new(config.clone()))
        .collect();

    for (row, btn) in button_pool.iter().enumerate() {
        let app = app.clone();
        let results = results.clone();
        let navigator = navigator.clone();

        btn.button.connect_clicked(move |_| {
            let items = results.borrow();
            let nav = navigator.borrow();
            let len = items.len();

            if row < max_rows.min(len) {
                let idx = (nav.offset() + row) % len.max(1);
                items[idx].handle.execute(&app);
            }
        });

        btn.button.set_visible(false);
        results_box.append(&btn.button);
    }

    let redraw_fn: Rc<dyn Fn()> = {
        let results = results.clone();
        let navigator = navigator.clone();
        let count_label = count_label.clone();
        let button_pool = Rc::new(button_pool);

        Rc::new(move || {
            let items = results.borrow();
            let nav = navigator.borrow();
            let len = items.len();

            count_label.set_text(&format!(
                "{} result{}",
                len,
                if len == 1 { "" } else { "s" }
            ));

            let visible = max_rows.min(len);
            let offset = nav.offset();
            let selected = nav.selected();

            for (i, btn) in button_pool.iter().enumerate() {
                if i < visible {
                    let idx = (offset + i) % len.max(1);
                    btn.update(&items[idx].item, i == selected);
                    btn.button.set_visible(true);
                } else {
                    btn.button.set_visible(false);
                }
            }
        })
    };

    {
        let plugins = plugins.clone();
        let results = results.clone();
        let navigator = navigator.clone();
        let redraw_fn = redraw_fn.clone();
        let in_actions_mode = in_actions_mode.clone();
        let last_query = last_query.clone();

        search.connect_changed(move |entry| {
            if *in_actions_mode.borrow() {
                return;
            }

            let query = entry.text().to_string();
            *last_query.borrow_mut() = query.clone();

            let matched = collect_results(&plugins, &query);
            let len = matched.len();
            navigator.borrow_mut().set_len(len);
            *results.borrow_mut() = matched;
            redraw_fn();
        });
    }

    setup_keybindings(
        &window,
        &search,
        navigator.clone(),
        redraw_fn.clone(),
        &results,
        plugins.clone(),
        in_actions_mode.clone(),
        last_query.clone(),
        app,
    );

    setup_scroll(&results_box, navigator.clone(), redraw_fn.clone(), max_rows);
    setup_focus(&window, app);
    apply_styling(&window, &config);

    {
        let matched = collect_results(&plugins, "");
        let len = matched.len();
        navigator.borrow_mut().set_len(len);
        *results.borrow_mut() = matched;
        redraw_fn();
    }

    play_fade_animation(&vbox, &window, &search);
}

fn collect_results(plugins: &[Box<dyn Plugin>], query: &str) -> Vec<SearchResult> {
    plugins
        .iter()
        .enumerate()
        .filter(|(_, plugin)| plugin.can_handle(query))
        .flat_map(|(plugin_idx, plugin)| {
            plugin
                .search(query)
                .into_iter()
                .enumerate()
                .map(move |(item_idx, (item, handle))| SearchResult {
                    item,
                    handle,
                    plugin_idx,
                    item_idx,
                })
        })
        .collect()
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

fn setup_keybindings(
    window: &ApplicationWindow,
    search: &SearchEntry,
    navigator: Rc<RefCell<Navigator>>,
    redraw: Rc<dyn Fn()>,
    results: &Rc<RefCell<Vec<SearchResult>>>,
    plugins: Rc<Vec<Box<dyn Plugin>>>,
    in_actions_mode: Rc<RefCell<bool>>,
    last_query: Rc<RefCell<String>>,
    app: &Application,
) {
    use gtk4::gdk::ModifierType;
    use gtk4::glib::{self, Propagation};
    use gtk4::{EventControllerKey, PropagationPhase};

    let app_clone = app.clone();
    let results_clone = results.clone();
    let navigator_clone = navigator.clone();
    let search_clone = search.clone();

    let key_ctrl = EventControllerKey::new();
    key_ctrl.set_propagation_phase(PropagationPhase::Capture);
    key_ctrl.set_propagation_limit(gtk4::PropagationLimit::None);

    key_ctrl.connect_key_pressed(move |_, keyval, _, state| match keyval.name().as_deref() {
        Some("Up") => {
            navigator.borrow_mut().move_selection(-1);
            redraw();
            Propagation::Stop
        }
        Some("Down") | Some("Tab") => {
            navigator.borrow_mut().move_selection(1);
            redraw();
            Propagation::Stop
        }
        Some("Home") => {
            navigator.borrow_mut().jump_to_start();
            redraw();
            Propagation::Stop
        }
        Some("End") => {
            navigator.borrow_mut().jump_to_end();
            redraw();
            Propagation::Stop
        }

        Some("BackSpace") => {
            if state.contains(ModifierType::SHIFT_MASK) && *in_actions_mode.borrow() {
                *in_actions_mode.borrow_mut() = false;

                let search_clone = search_clone.clone();
                let last_query = last_query.clone();
                let plugins = plugins.clone();
                let navigator_clone = navigator_clone.clone();
                let results_clone = results_clone.clone();
                let redraw = redraw.clone();

                glib::idle_add_local(move || {
                    search_clone.grab_focus();
                    let query = last_query.borrow().clone();
                    search_clone.set_text(&query);

                    let matched = collect_results(&plugins, &query);
                    let len = matched.len();
                    navigator_clone.borrow_mut().set_len(len);
                    *results_clone.borrow_mut() = matched;
                    redraw();

                    glib::ControlFlow::Break
                });

                return Propagation::Stop;
            }

            Propagation::Proceed
        }

        Some("Escape") => {
            app_clone.quit();
            Propagation::Stop
        }

        Some("Return") => {
            let nav = navigator_clone.borrow();
            if nav.len() > 0 {
                let idx = nav.current_index();

                if state.contains(ModifierType::SHIFT_MASK) && !*in_actions_mode.borrow() {
                    let (plugin_idx, item_idx) = {
                        let items = results_clone.borrow();
                        (items[idx].plugin_idx, items[idx].item_idx)
                    };
                    let query = last_query.borrow().clone();
                    let actions: Vec<_> = plugins[plugin_idx]
                        .get_actions(&query, item_idx)
                        .into_iter()
                        .enumerate()
                        .map(|(item_idx, (item, handle))| SearchResult {
                            item,
                            handle,
                            plugin_idx,
                            item_idx,
                        })
                        .collect();

                    if !actions.is_empty() {
                        *in_actions_mode.borrow_mut() = true;
                        search_clone.set_text("");

                        let len = actions.len();
                        drop(nav);
                        navigator_clone.borrow_mut().set_len(len);
                        *results_clone.borrow_mut() = actions;
                        redraw();
                        return Propagation::Stop;
                    }
                }

                let items = results_clone.borrow();
                items[idx].handle.execute(&app_clone);
                drop(items);
                drop(nav);

                let query = search_clone.text().to_string();
                let matched = collect_results(&plugins, &query);
                let len = matched.len();
                navigator_clone.borrow_mut().set_len(len);
                *results_clone.borrow_mut() = matched;
                redraw();
            }
            Propagation::Stop
        }

        _ => Propagation::Proceed,
    });

    window.add_controller(key_ctrl);
}

fn setup_scroll(
    results_box: &GtkBox,
    navigator: Rc<RefCell<Navigator>>,
    redraw: Rc<dyn Fn()>,
    max_rows: usize,
) {
    use gtk4::glib::Propagation;
    use gtk4::EventControllerScroll;

    let scroll_ctrl = EventControllerScroll::new(gtk4::EventControllerScrollFlags::VERTICAL);
    scroll_ctrl.connect_scroll(move |_, _, dy| {
        if navigator.borrow().len() > max_rows {
            if dy > 0.0 {
                navigator.borrow_mut().move_selection(1);
            } else if dy < 0.0 {
                navigator.borrow_mut().move_selection(-1);
            }
            redraw();
            Propagation::Stop
        } else {
            Propagation::Proceed
        }
    });
    results_box.add_controller(scroll_ctrl);
}

fn setup_focus(window: &ApplicationWindow, app: &Application) {
    use gtk4::EventControllerFocus;

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
    let border_color = config
        .style
        .as_ref()
        .and_then(|s| s.border_color.as_deref())
        .unwrap_or("@accent_bg_color");
    let text_color = config
        .style
        .as_ref()
        .and_then(|s| s.text_color.as_deref())
        .unwrap_or("@window_fg_color");
    let font_css = config
        .style
        .as_ref()
        .and_then(|s| s.font.as_deref())
        .map(font_to_css)
        .unwrap_or_default();

    let css_provider = gtk4::CssProvider::new();
    css_provider.load_from_data(&format!(
        "window {{ border-radius: {}px; border: {}px solid {}; }} window, label, button, entry {{ {} }} label, entry {{ color: {}; }}",
        border_radius, border_width, border_color, font_css, text_color
    ));

    let display = gtk4::prelude::WidgetExt::display(window);
    gtk4::style_context_add_provider_for_display(
        &display,
        &css_provider,
        gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION,
    );
}

fn font_to_css(font: &str) -> String {
    let font = font.trim();
    let Some((family, size)) = font.rsplit_once(' ') else {
        return format!("font-family: '{}';", css_string(font));
    };

    if size.parse::<f32>().is_ok() {
        format!(
            "font-family: '{}'; font-size: {}pt;",
            css_string(family.trim()),
            size
        )
    } else {
        format!("font-family: '{}';", css_string(font))
    }
}

fn css_string(value: &str) -> String {
    value.replace('\\', "\\\\").replace('\'', "\\'")
}

fn play_fade_animation(vbox: &GtkBox, window: &ApplicationWindow, search: &SearchEntry) {
    vbox.set_opacity(0.0);
    window.present();

    let target = adw::CallbackAnimationTarget::new({
        let vbox = vbox.clone();
        move |value| vbox.set_opacity(value)
    });

    let animation = adw::TimedAnimation::new(vbox, 0.0, 1.0, 150, target);
    adw::prelude::AnimationExt::play(&animation);
    search.grab_focus();
}
