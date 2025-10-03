# 🚀 Launchery

<img src="assets/demo.gif" width="640" alt="Launchery demo showcase"/>

> _“Whipped this up because no other launcher did what I wanted.”_  
> _Rust skills: questionable. Result: usable._

A lightweight GTK4 app launcher for Wayland.  
Built with [`gtk4`](https://gtk-rs.org/), [`libadwaita`](https://gnome.pages.gitlab.gnome.org/libadwaita/), and [`gtk4-layer-shell`](https://github.com/wmww/gtk4-layer-shell-rs).

---

## ✨ Features

- Fast fuzzy search ([fuzzy-matcher](https://crates.io/crates/fuzzy-matcher))
- Configurable styles via `config.toml`
- App icons + live match highlighting
- Smooth animations (libadwaita)
- Keyboard + mouse controls:
  - Arrows / Tab → move
  - Enter → launch
  - Esc → quit
  - Scroll → browse
- Wayland-native overlay window (`gtk4-layer-shell`)

---

## 📂 Config

Launchery reads from a `config.toml`:

- **Dev mode** → `./config.toml`
- **Release** → `~/.config/launchery/config.toml`

### Example `config.toml`

```toml
[highlight]
color = "red"

[style]
border_radius = 12
border_width  = 2

[window]
width = 540
max_rows = 5
```
