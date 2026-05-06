pub mod actions;
pub mod app_launcher;
pub mod scripts;

use gtk4::{gio, Application};

#[derive(Clone)]
pub enum DisplayIcon {
    Named(String),
    File(String),
    GIcon(gio::Icon),
}

impl DisplayIcon {
    pub fn named(icon_name: impl Into<String>) -> Self {
        Self::Named(icon_name.into())
    }
}

#[derive(Clone)]
pub struct DisplayItem {
    pub label: String,
    pub icon: DisplayIcon,
    pub highlight_indices: Option<Vec<usize>>,
}

pub struct MatchHandle {
    executor: Box<dyn Fn(&Application)>,
}

impl MatchHandle {
    pub fn new(executor: impl Fn(&Application) + 'static) -> Self {
        Self {
            executor: Box::new(executor),
        }
    }

    pub fn execute(&self, app: &Application) {
        (self.executor)(app);
    }
}

pub trait Plugin {
    #[allow(dead_code)]
    fn name(&self) -> &str;
    fn can_handle(&self, query: &str) -> bool;
    fn search(&self, query: &str) -> Vec<(DisplayItem, MatchHandle)>;

    fn get_actions(&self, _query: &str, _item_index: usize) -> Vec<(DisplayItem, MatchHandle)> {
        Vec::new()
    }
}
