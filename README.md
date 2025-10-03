# 🚀 Launchery

<img src="assets/demo.gif" width="640" alt="Launchery demo showcase"/>

> _"Made for me cause no other out there was doing what I wanted — and I'm picky."_  
> _PS: I'm noob at Rust, this was hacked together in a day._

A lightweight, **(and hopefully fast)** GTK4 application launcher for Wayland.  
Built with [`gtk4`](https://gtk-rs.org/), [`libadwaita`](https://gnome.pages.gitlab.gnome.org/libadwaita/), and [`gtk4-layer-shell`](https://github.com/wmww/gtk4-layer-shell-rs),  
it brings a modern app launcher experience that integrates seamlessly into your desktop.

---

## ✨ Features

- **Instant fuzzy search** powered by [fuzzy-matcher](https://crates.io/crates/fuzzy-matcher).
- **Customizable styles** via `config.toml` (highlight color, border radius, border width, etc.).
- **App icons & highlighting** with fuzzy-match highlights in real-time.
- **Smooth animations** thanks to `libadwaita` timed animations.
- **Keyboard & mouse navigation**:
- **Tab / Arrow keys** to move selection
- **Enter** to launch
- **Escape** to quit
- **Scroll** through results
- **Wayland-native** overlay window with `gtk4-layer-shell`.

---

## 📂 Configuration

The launcher reads settings from a `config.toml` file.

- In **development/debug mode** → `./config.toml`
- In **release mode** → `~/.config/launchery/config.toml`

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
