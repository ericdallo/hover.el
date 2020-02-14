[![MELPA](https://melpa.org/packages/hover-badge.svg)](https://melpa.org/#/hover)

# hover.el

__Emacs tool for working with [hover](https://github.com/go-flutter-desktop/hover) for flutter__

## Installation

You can install from MELPA with package.el:

```
M-x package-install hover
```

## Configuration

| Variable  |  Description  | Default value |
| ------------------- | ------------------- | ----------------|
| `hover-command-path` |  Path to the hover executable command | tries to use `hover` if exists in $PATH |
|  `flutter-sdk-path` |  Path to flutter sdk path to find flutter executable command | tries to find `flutter` executable in $PATH |


__thanks to [flutter.el](https://github.com/amake/flutter.el) which inspired this project__
