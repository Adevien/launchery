# Launchery

<img src="assets/demo.gif" width="640" alt="Launchery demo"/>

Small GTK4 launcher for Wayland.

It searches desktop apps, shows fuzzy matches, and can also run small shell scripts from your Launchery config dir.

## What it does

- Finds installed desktop apps
- Shows app icons
- Highlights fuzzy matches
- Opens apps with keyboard or mouse
- Supports secondary app actions with `Shift+Enter`
- Loads named scripts from `~/.config/launchery/scripts`

## Controls

- `Up` / `Down`: move selection
- `Tab`: next result
- `Enter`: run selected item
- `Shift+Enter`: actions menu
- `Shift+Backspace`: leave actions menu
- `Esc`: quit
- Mouse wheel: scroll

## Config

Debug builds read:

```text
./config.toml
```

Release builds read:

```text
~/.config/launchery/config.toml
```

Example:

```toml
[highlight]
color = "red"

[style]
border_radius = 12
border_width = 2
border_color = "@accent_bg_color"
text_color = "@window_fg_color"
font = "JetBrains Mono 11"

[window]
width = 540
max_rows = 5

[application]
editor = "your-editor"
terminal = "your-terminal"

[scripts]
allowed_shebangs = ["#!/bin/bash", "#!/usr/bin/env bash"]
```

`editor` and `terminal` are used by actions and terminal scripts. `allowed_shebangs` controls which script interpreters Launchery accepts.

## Scripts

Put scripts here:

```text
~/.config/launchery/scripts
```

Only `.sh` files are loaded. A script needs an allowed shebang and a `#NAME=` line:

```sh
#!/bin/bash
#NAME=GYMLETE SSH
#TERMINAL
```

The name is what you search for in Launchery. Scripts run directly by default. Add `#TERMINAL` to open one in your configured terminal.

## Build

Install GTK4, libadwaita, and layer shell development packages for your distro.

```sh
cargo build --release
```

For development:

```sh
cargo run
```
