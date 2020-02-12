;;; hover.el --- package to use hover with flutter   -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: Eric Dallo
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.5"))
;; Keywords: hover, flutter, mobile
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

(defconst hover-buffer-name "*Hover*")

(defvar hover-command-path (concat (getenv "GOPATH") "/bin/hover")
  "Path to go where hover is installed.")

(defvar flutter-sdk-path nil
  "Path to flutter SDK.")

;;; Internal

(defun hover--project-get-root ()
  "Find the root of the current project."
  (or (locate-dominating-file default-directory "pubspec.yaml")
      (error "This does not appear to be a Flutter project (pubspec.yaml not found)")))

(defmacro hover--from-project-root (&rest body)
  "Execute BODY with cwd set to the project root."
  `(let ((root (hover--project-get-root)))
     (if root
         (let ((default-directory root))
           ,@body)
       (error "Root of flutter project not found"))))

(defun hover--running-p ()
  "Return non-nil if the `hover` process is already running."
  (comint-check-proc hover-buffer-name))

(defmacro hover--with-run-proc (args &rest body)
  "Execute BODY while ensuring an inferior `hover` process is running.
ARGS is a space-delimited string of CLI flags passed to
`hover`, and can be nil."
  `(hover--from-project-root
    (let* ((buffer (get-buffer-create hover-buffer-name))
           (alive (hover--running-p))
           (arglist (if ,args (split-string ,args))))
      (unless alive
        (apply #'make-comint-in-buffer "Hover" buffer (build-hover-command) nil "run" arglist))
      (with-current-buffer buffer
        (unless (derived-mode-p 'hover-mode)
          (hover-mode)))
      ,@body)))

(defun hover--initialize ()
  "Helper function to initialize Hover."
  (setq comint-process-echoes nil)
  (when flutter-sdk-path
    (let ((flutter-command-path (concat (file-name-as-directory flutter-sdk-path) "bin")))
      (setenv "PATH" (concat flutter-command-path ":" (getenv "PATH"))))))

;;; Public interface

(defun build-hover-command ()
  "Check if command exists and return the hover command."
  (if (file-exists-p hover-command-path)
      hover-command-path
    (error (format "Hover command not found in go path '%s'. Try to configure `hover-command-path`" hover-command-path))))

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
(define-derived-mode hover-mode comint-mode "Hover"
  "Major mode for `hover-run'."
  (setq comint-prompt-read-only t))

(add-hook 'hover-mode-hook #'hover--initialize)

(provide 'hover)
;;; hover.el ends here
