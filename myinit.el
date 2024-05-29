(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  (expand-file-name (convert-standard-filename "eln-cache/")
			    user-emacs-directory)))

(setq inhibit-startup-screen t)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq message-log-max 100000)

(setq line-move-visual t) ;; C-n C-p move by screenlines
(setq track-eol nil) ;; don't track end of line when moving
(setq next-line-add-newline nil) ;; C-n at the end of buffer won't add new lines

(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(size-indication-mode 1)
(setq what-cursor-show-names t)

(setq minibuffer-follows-selected-frame nil) ;; minibuffer stays in same frame
(file-name-shadow-mode 1) ;; shadow ignored file path in minibuffer
(setq insert-default-directory t) ;; strat with default directory in minibuffer
(setq max-mini-window-height 0.25) ;; default value, 25% of frame height

(setq resize-mini-windows t)

(setq enable-recursive-minibuffer t)
(setq minibuffer-depth-indicate-mode t)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq minibuffer-completion-auto-choose t) ;; insert current completion candidate in
(setq confirm-nonexistent-file-or-buffer nil) ;; don't ask confirmation

(setq completion-styles '(initials partial-completion flex basic))
(setq completion-auto-help nil) ;; don't show completion list buffer
(setq completion-auto-select nil) ;; don't switch to completion list buffer when displayed
(setq completion-cycle-threshold t) ;; always cycle through completion candidates
(setq completions-format 'horizontal) ;; completion list buffer format
(setq completions-sort 'alphabetical) ;; sort candidatate alphabetically
(setq completions-max-height nil) ;; no height limit for completion list buffer
(setq completions-header-format nil) ;; no header in completion list buffer

(setq minibuffer-eldef-shorten-default t)

(setq history-length 100
      history-delete-duplicates t
      savehist-minibuffer-history-variables '(minibuffer-history
					      query-replace-history
					      file-name-history
					      buffer-name-history
					      regexp-history
					      extended-command-history
					      shell-command-history
					      read-expression-history
					      command-history))

(setq isearch-resume-in-command-history t)

(setq use-short-answers t)

(setq suggest-key-bindings 5)
(setq extended-command-suggest-shorter t)

(setq help-window-select t) ;; switch to help window when created
(setq help-window-keep-selected t) ;; reuse same Help buffer

(setq highlight-nonselected-windows t) ;; each window highlights its own region
(setq use-empty-active-region nil) ;; region aware commands treat empty region as inactive

(setq delete-active-region 'kill) ;; kill region instead of delete
(setq mark-even-if-inactive nil)
(delete-selection-mode -1) ;; with active region typing character
;; inserts it without replacing region

(setq set-mark-command-repeat-pop t
      mark-ring-max 512
      global-mark-ring-max 512)

(unless (package-installed-p 'expand-region)
  (package-refresh-contents)
  (package-install 'expand-region))

(global-set-key (kbd "C-+") 'er/expand-region)
(global-set-key (kbd "C-_") 'er/contract-region)

(defun my/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'my/kill-region-or-backward-word)

(setq kill-do-not-save-duplicates t)

(setq kill-whole-line t)

(setq kill-read-only-ok t)

(setq kill-ring-max 1000)

(setq save-interprogram-paste-before-kill t)

(setq register-preview-delay 2) ;; seconds before displaying preview of register list

(setq bookmark-save-flag 1)

(setq next-screen-context-lines 3) ;; number lines that overlap during scroll command

(setq scroll-conservatively 1000)

(setq hscroll-margin 2)
(setq hscroll-step 2)
(put 'scroll-left 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(set-face-attribute 'default nil :height 120)

(setq hi-lock-auto-select-face t)

(setq show-trailing-whitespace t)
(setq-default indicate-empty-lines t) ;; show blank lines at the end of buffer

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(setq what-cursor-show-names t)

(setq line-number-display-limit nil)

(setq blink-cursor-blink -1)

(setq display-line-numbers-type t)
(setq display-line-numbers-width t)
(global-display-line-numbers-mode 1)
(setq display-raw-bytes-as-hex t)
(setq visible-bell t)

(setq search-ring-max 1000)

(setq search-exit-option t
      isearch-allow-scroll t)

;;  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;;  (global-set-key (kbd "M-%") 'query-replace-regexp)
(setq regexp-search-ring-max 1000
      search-default-mode t) ;; default regex search

(setq isearch-lazy-count t)

(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-personal-dictionary (expand-file-name "dictionary" user-emacs-directory))

(setq kmacro-ring-max 1000)

(if (file-exists-p (expand-file-name "macros" user-emacs-directory))
    (load-file (expand-file-name "macros" user-emacs-directory)))

(setq make-backup-files nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(setq auto-revert-verbose nil ;; don't flash echo area message
      global-auto-revert-non-file-buffers nil ;; disable auto revert for dired buffers etc
      auto-revert-remote-files nil)
(global-auto-revert-mode nil)

(make-directory (expand-file-name "autosave/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "autosave/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))
(setq delete-auto-save-files t
      kill-buffer-delete-auto-save-files t)

(setq delete-by-moving-to-trash t)

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(setq completion-styles '(initials partial-completion flex basic))
(if (>= emacs-major-version 29)
    (progn
      (icomplete-vertical-mode 1)
      (fido-vertical-mode 1)))

(setq help-window-select t ;; select help window
      ;; in strongly dedicate windows behave like pop-to-buffer
      switch-to-buffer-in-dedicated-window 'pop
      ;; C-x C-b respects display buffer rules
      switch-to-buffer-obey-display-actions t)
;; switch to occur buffer immediately
(add-hook 'occur-hook
	  '(lambda ()
	     (switch-to-buffer-other-window "*Occur*")))
;; switch to compilation buffer immediately
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)
(setq display-buffer-alist
      '(("\\*\\(Metahelp\\|info\\|Help\\|Apropos\\).*"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (window-width . 0.5)
	 (slot . 0))
	("\\*\\(.*shell\\|ansi-term\\|\.*eshell\\|.*terminal\\|Async Shell\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Messages\\|Output\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(vc-\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(log-edit-\\).*"
	 (display-buffer-in-atom-window)
	 (side . right)
	 (window-width . 0.3)
	 (slot . 0))
	("\\*\\(Diff\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Open Recent\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Ibuffer\\).*"
	 (display-buffer-in-side-window)
	 (side . right)
	 (window-width . 0.5)
	 (slot . 0))
	("\\*\\(Embark\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(eldoc\\|xref\\|Flymake\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Python\\|ielm\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(compilation\\|Occur\\|grep\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))))
(global-set-key (kbd "<f12>") 'window-toggle-side-windows)

(require 'scroll-bar)
(scroll-bar-mode -1)
(setq horizontal-scroll-bar-mode -1)

(menu-bar-mode -1)
(setq tty-menu-open-use-tmm t)

(require 'tool-bar)
(tool-bar-mode -1)

(setq tab-bar-show 1)

(setq use-dialog-box nil)

(tooltip-mode -1)

(setq tab-always-indent 'complete)
;; distance between tab stops in columns. control width of tab characters to display
;; it should be positive integer and default is 8
(setq tab-width 8)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'org-tempo)

(org-babel-do-load-languages 'org-babel-load-languages
			     '((C . t)
			       (java . t)
			       (latex . t)
			       (lua . t)
			       (python . t)
			       (shell . t)
			       (emacs-lisp . t)))
(setq org-confirm-babel-evaluate nil)

(setq imenu-auto-rescan t)

(which-function-mode 1)

(require 'package)
(package-initialize)
(if (< emacs-major-version 28)
    (add-to-list 'package-archives
		 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

;;    (add-to-list 'package-archives
;;		 '("melpa" . "https://stable.melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))
