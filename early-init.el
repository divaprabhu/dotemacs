;;; early-init.el  --- Early Init  -*- lexical-binding: t; -*-
;;
;; Copied from Emacs Solo
;;

;;; Commentary:
;;  Early init configuration for Emacs Solo
;;

;;; Code:


;;; -------------------- PERFORMANCE & HACKS
;; HACK: inscrease startup speed

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

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31

;; Always start Emacs and new frames maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      )

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))


;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))


(provide 'early-init)
;;; early-init.el ends here
