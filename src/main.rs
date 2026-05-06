mod config;
mod navigator;
mod plugins;
mod ui;

use gtk4::gio;
use gtk4::prelude::*;
use gtk4::Application;
use std::rc::Rc;

use config::Config;
use ui::window::build_app_window;

fn main() {
    std::env::set_var("GDK_BACKEND", "wayland");
    std::env::set_var("GSK_RENDERER", "cairo");

    adw::init().expect("error adw could not load");
    let config = Rc::new(Config::load());

    let app = Application::builder()
        .application_id("com.adv.Launchery")
        .flags(gio::ApplicationFlags::HANDLES_COMMAND_LINE)
        .build();

    {
        let config = config.clone();
        app.connect_activate(move |app| {
            build_app_window(app, config.clone());
        });
    }

    {
        app.connect_command_line(move |app, _cmd| {
            app.activate();
            0
        });
    }

    app.run();
}
