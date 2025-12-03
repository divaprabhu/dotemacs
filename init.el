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

(setq inhibit-compacting-font-caches t)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
;; (setq warning-minimum-level :error)
;; (setq warning-suppress-types '((lexical-binding)))

;; Avoid littering
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  (expand-file-name (convert-standard-filename "eln-cache/")
			    user-emacs-directory)))

;; Initialize Packages
(require 'package)
;; (add-to-list 'package-archives
             ;; '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)			; make sure to update load-path for downloaded packages
(org-babel-load-file "~/.config/emacs/myinit.org")

