;;; hover.el --- Package to use hover with flutter   -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: Eric Dallo
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.5"))
;; Keywords: hover, flutter, mobile, tools
;; URL: https://github.com/ericdallo/hover.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; hover.el is a package for running the `hover' binary tool from
;; https://github.com/go-flutter-desktop/hover interactively.
;; It is most useful when paired with `dart-mode'.

;;; Code:

(require 'comint)

(defgroup hover nil
  "Package to use hover with flutter."
  :group 'tools)

(defcustom hover-buffer-name "*Hover*"
  "Buffer name for hover."
  :type 'string
  :group 'hover)

(defcustom hover-command-path nil
  "Path to hover command."
  :type 'string
  :group 'hover)

(defcustom hover-flutter-sdk-path nil
  "Path to flutter SDK."
  :type 'string
  :group 'hover)

(defcustom hover-hot-reload-on-save nil
  "If non-nil, triggers hot-reload on buffer save."
  :type '(choice (const nil) (other t))
  :group 'hover)

(defvar hover-mode-map (copy-keymap comint-mode-map)
  "Basic mode map for `hover-run'.")

;;; Internal

(defmacro hover--with-run-proc (args &rest body)
  "ARGS is a space-delimited string of CLI flags passed to `hover`.
Execute BODY while ensuring an inferior `hover` process is running."
  `(hover--from-project-root
    (let* ((buffer (get-buffer-create hover-buffer-name))
           (alive (hover--running-p))
           (arglist (when ,args (split-string ,args))))
      (unless alive
        (apply #'make-comint-in-buffer "Hover" buffer (hover-build-hover-command) nil "run" arglist))
      (with-current-buffer buffer
        (unless (derived-mode-p 'hover-mode)
          (hover-mode)))
      ,@body)))

(defmacro hover--from-project-root (&rest body)
  "Execute BODY with cwd set to the project root."
  `(let ((root (hover--project-get-root)))
     (if root
         (let ((default-directory root))
           ,@body)
       (error "Root of flutter project not found"))))

(defun hover--make-interactive-function (key name)
  "Define a function that sends KEY to the `hover` process.
The function's name will be NAME prefixed with 'hover-'."
  (let* ((name-str (symbol-name name))
         (funcname (intern (concat "hover-" name-str))))
    (defalias funcname
      `(lambda ()
         ,(format "Send key '%s' to inferior hover to invoke '%s' function." key name-str)
         (interactive)
         (hover--send-command ,key)))))

(defun hover--send-command (command)
  "Send COMMAND to a running hover process."
  (hover--with-run-proc
   nil
   (let ((proc (get-buffer-process hover-buffer-name)))
     (comint-send-string proc command))))

(defun hover--project-get-root ()
  "Find the root of the current project."
  (or (locate-dominating-file default-directory "go")
      (error "This does not appear to be a Hover project (go folder not found), did you already run `hover init`?")))

(defun hover--running-p ()
  "Return non-nil if the `hover` process is already running."
  (comint-check-proc hover-buffer-name))

(defun hover--run-command-on-hover-buffer (command)
  "Pop hover buffer window and run COMMAND."
  (let ((current (current-buffer)))
    (pop-to-buffer hover-buffer-name nil t)
    (select-window (get-buffer-window current)))
  (hover--send-command command))

(defun hover--hot-reload ()
    "Hot reload hover if it is already running."
  (when (hover--running-p)
    (hover--run-command-on-hover-buffer "r")))

;;; Key bindings

(defconst hover-interactive-keys-alist
  '(("r" . hot-reload)
    ("R" . hot-restart)
    ("h" . help)
    ("w" . widget-hierarchy)
    ("t" . rendering-tree)
    ("L" . layers)
    ("S" . accessibility-traversal-order)
    ("U" . accessibility-inverse-hit-test-order)
    ("i" . inspector)
    ("p" . construction-lines)
    ("o" . operating-systems)
    ("z" . elevation-checker)
    ("P" . performance-overlay)
    ("a" . timeline-events)
    ("s" . screenshot)
    ("d" . detatch)
    ("q" . quit)))

(defun hover-register-key (key name)
  "Register a KEY with NAME recognized by the `hover` process.
A function `hover-NAME' will be created that sends the key to
the `hover` process."
  (let ((func (hover--make-interactive-function key name)))
    (define-key hover-mode-map key func)))

(defun hover-register-keys (key-alist)
  "Call `hover-register-key' on all (key . name) pairs in KEY-ALIST."
  (dolist (item key-alist)
    (hover-register-key (car item) (cdr item))))

(hover-register-keys hover-interactive-keys-alist)

;;; Public interface

(defun hover-build-hover-command ()
  "Check if command exists and return the hover command."
  (if hover-command-path
      hover-command-path
    (if (executable-find "hover")
        "hover"
      (error (format "Hover command not found in go path '%s'. Try to configure `hover-command-path`" hover-command-path)))))

;;;###autoload
(defun hover-run (&optional args)
  "Execute `hover run` inside Emacs.

ARGS is a space-delimited string of CLI flags passed to
`hover`, and can be nil.  Call with a prefix to be prompted for
args."
  (interactive
   (list (when current-prefix-arg
           (read-string "Args: "))))
  (hover--with-run-proc
   args
   (pop-to-buffer-same-window buffer)))

;;;###autoload
(defun hover-run-or-hot-reload ()
  "Start `hover run` or hot-reload if already running."
  (interactive)
  (if (hover--running-p)
      (hover--run-command-on-hover-buffer "r")
    (hover-run)))

;;;###autoload
(defun hover-run-or-hot-restart ()
  "Start `hover run` or hot-restart if already running."
  (interactive)
  (if (hover--running-p)
     (hover--run-command-on-hover-buffer "R")
    (hover-run)))

;;;###autoload
(define-derived-mode hover-mode comint-mode "Hover"
  "Major mode for `hover-run'."
  (setq comint-prompt-read-only t)
  (setq comint-process-echoes nil)
  (when hover-flutter-sdk-path
    (let ((flutter-command-path (concat (file-name-as-directory hover-flutter-sdk-path) "bin")))
      (setenv "PATH" (concat flutter-command-path ":" (getenv "PATH")))))
  (when hover-hot-reload-on-save
    (add-hook 'after-save-hook 'hover--hot-reload)))

(provide 'hover)
;;; hover.el ends here
