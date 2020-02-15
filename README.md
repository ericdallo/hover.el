<img align="right"  src="https://github.com/go-flutter-desktop/hover/blob/master/assets/app/icon.png" width="64"/>

[![MELPA](https://melpa.org/packages/hover-badge.svg)](https://melpa.org/#/hover)

# hover.el

_Emacs tool for working with [hover](https://github.com/go-flutter-desktop/hover) for **flutter**_

## Installation

You can install from MELPA with package.el:

```
M-x package-install hover
```

## Running
**hover.el** helps you run the `hover` binary interactively as an inferior process. It's designed to work together with `dart-mode`.
For example you can bind `hover-run-or-hot-reload` to `C-M-z` in dart-mode. While editing your Dart code, just hit `C-M-z` to either run your app, or if it's already running, to hot-reload it.

## Configuration

| Variable  |  Description  | Default value |
| ------------------- | ------------------- | ----------------|
| `hover-command-path` |  Path to the hover executable command | tries to use `hover` if exists in $PATH |
|  `hover-flutter-sdk-path` |  Path to flutter sdk path to find flutter executable command | tries to find `flutter` executable in $PATH |
| `hover-hot-reload-on-save` | On buffer save, triggers hover hot-reload (if hover is running) | nil |

# Example

The following example uses all available configurations above, you can customize as you wish.

```elisp
(use-package hover
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-z" . #'hover-run-or-hot-reload)
              ("C-M-x" . #'hover-run-or-hot-restart))
  :custom
  (setq flutter-sdk-path (concat (getenv "HOME") "/flutter") ; remove if `flutter` is already in $PATH
        hover-command-path (concat (getenv "GOPATH") "/bin/hover") ; remove if `hover` is already in $PATH
        hover-hot-reload-on-save t))
```


_thanks to [flutter.el](https://github.com/amake/flutter.el) which inspired this project_
