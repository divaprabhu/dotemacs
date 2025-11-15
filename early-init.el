;;; early-init.el --- Emacs Solo (no external packages) Configuration --- Early Init  -*- lexical-binding: t; -*-

;;; Commentary:
;;  Early init configuration for Emacs Solo
;; https://github.com/LionyxML/emacs-solo
;;

;;; Code:

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;; Window and Frames
;; Always start Emacs and new frames maximized

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat " "
                      (file-name-nondirectory (directory-file-name (project-root project))))
              (concat " " (buffer-name))))))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))
(setq message-log-max 100000)


;; Avoid littering
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq multisession-directory (expand-file-name "multisession" user-emacs-directory))
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
      (expand-file-name (convert-standard-filename "eln-cache/")
                user-emacs-directory)))


;; Initialize Packages
(require 'package)

(package-initialize)			; make sure to update load-path for downloaded packages
;; (org-babel-load-file "~/.config/emacs/myinit.org")

(provide 'early-init)
;;; early-init.el ends here
