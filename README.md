<img align="right"  src="https://github.com/go-flutter-desktop/hover/blob/master/assets/app/icon.png" width="64"/>

[![MELPA](https://melpa.org/packages/hover-badge.svg)](https://melpa.org/#/hover)
[![Actions Status](https://github.com/ericdallo/hover.el/workflows/Tests%20CI/badge.svg)](https://github.com/ericdallo/hover.el/actions)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0286115a90df4301a84890d31c02b55b)](https://www.codacy.com/manual/ericdallo/hover.el?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=ericdallo/hover.el&amp;utm_campaign=Badge_Grade)

# hover.el

_Emacs tool for running **flutter** mobile apps on **desktop** using [hover](https://github.com/go-flutter-desktop/hover)._

<img src="https://github.com/ericdallo/hover.el/blob/screenshots/usage.gif"/>

If you want to run flutter on a emulator from Emacs, you should check [flutter.el](https://github.com/amake/flutter.el).

## Installation

You can install from MELPA with package.el:

```bash
M-x package-install hover
```

## Running
**hover.el** helps you run the `hover` binary interactively as an inferior It's. process designed to work together with `dart-mode`.
For example you can bind `hover-run-or-hot-reload` to `C-M-z` in dart-mode. While editing your Dart code, just hit `C-M-z` to either run your app, or if it's already running, to hot-reload it.

## Configuration

| Variable                            | Description                                                     | Default value                               |
| --------------------------          | --------------------------------------------------------------- | ------------------------------------------- |
| `hover-command-path`                | Path to the hover executable command                            | tries to use `hover` if exists in $PATH     |
| `hover-flutter-sdk-path`            | Path to flutter sdk path to find flutter executable command     | tries to find `flutter` executable in $PATH |
| `hover-hot-reload-on-save`          | On buffer save, triggers hover hot-reload (if hover is running) | `nil`                                       |
| `hover-screenshot-path`             | If non-nil, save hover screenshot on specified folder.          | project root                                |
| `hover-screenshot-prefix`           | Prefix for file name on `hover-take-screenshot`.                | hover-                                      |
| `hover-observatory-uri`             | Hover custom observatory-uri.                                   | `http://127.0.0.1:50300`                    |
| `hover-clear-buffer-on-hot-restart` | Calls `hover-clear-buffer` after a `hover-hot-restart`          | `nil`                                       |

## Example

The following example uses **all available** configurations above, you can customize as you wish.

```elisp
;; Assuming usage with dart-mode
(use-package dart-mode
  :custom
  (dart-sdk-path (concat (getenv "HOME") "/flutter/bin/cache/dark-sdk/")
   dart-format-on-save t))

(use-package hover
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-z" . #'hover-run-or-hot-reload)
              ("C-M-x" . #'hover-run-or-hot-restart)
              ("C-M-p" . #'hover-take-screenshot'))
  :init
  (setq hover-flutter-sdk-path (concat (getenv "HOME") "/flutter") ; remove if `flutter` is already in $PATH
        hover-command-path (concat (getenv "GOPATH") "/bin/hover") ; remove if `hover` is already in $PATH
        hover-hot-reload-on-save t
        hover-screenshot-path (concat (getenv "HOME") "/Pictures"
        hover-screenshot-prefix "my-prefix-"
        hover-observatory-uri "http://my-custom-host:50300"
        hover-clear-buffer-on-hot-restart t)))
```

_Thanks to [flutter.el](https://github.com/amake/flutter.el) which inspired this project._
