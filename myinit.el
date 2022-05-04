(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq inhibit-startup-screen t)

(fset 'yes-or-no-p 'y-or-n-p)
    (setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	 kill-buffer-query-functions))

(global-set-key (kbd "M-o") 'other-window)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(package-initialize)
(if (< emacs-major-version 28)
    (add-to-list 'package-archives
		 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

(setq next-line-add-newline t)

(line-number-mode 1)
(column-number-mode 1)
(hl-line-mode 1)
(size-indication-mode 1)
(setq what-cursor-show-names t)

(setq resize-mini-windows t)

(setq enable-recursive-minibuffer t)
(setq minibuffer-depth-indicate-mode t)

(setq completion-cycle-threshold t)

(setq minibuffer-eldef-shorten-default t)

(setq history-length 100)
(setq history-delete-duplicates t)

(setq isearch-resume-in-command-history t)

(setq suggest-key-bindings 5)
(setq extended-command-suggest-shorter t)

(setq highlight-nonselected-windows t)

(setq mark-even-inactive nil)

(setq set-mark-command-repeat-pop t)

(unless (package-installed-p 'expand-region)
  (package-refresh-contents)
  (package-install 'expand-region))

(global-set-key (kbd "C-+") 'er/expand-region)

(defun kill-region-or-backward-word ()
   (interactive)
   (if (region-active-p)
       (kill-region (region-beginning) (region-end))
     (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(setq kill-do-not-save-duplicates t)

(setq save-interprogram-paste-before-kill t)

(setq bookmark-save-flag 1)

(setq scroll-conservatively 2)

(setq hscroll-step 2)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq hi-lock-auto-select-face t)

(setq show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(setq what-cursor-show-names t)

(setq line-number-display-limit nil)

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		(:propertize evil-mode-line-tag face modus-themes-intense-magenta)
		(:propertize mode-name face mode-line-highlight)
		" (%l, %c)["
		(:eval (number-to-string (count-lines (point-min) (point-max))))
		"] ["
		(:eval (if (buffer-modified-p)
			   (format "%s" "MD")
			 (format "%s" "")))
		":"
		(:eval (if (eql buffer-read-only t)
			   (format "%s" "RD")
			 (format "%s" "")))
		"]"
		" ["
		(:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
			 (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
				(format "%s" "UTF-8"))
			       (t (upcase (symbol-name (plist-get sys :name)))))))
		":"
		(:eval (pcase (coding-system-eol-type buffer-file-coding-system)
			 (0 "LF")
			 (1 "CRLF")
			 (2 "CR")))
		":"
		current-input-method-title
		"] "
		(:propertize mode-line-buffer-identification
			 face modus-themes-intense-red
			 help-echo (buffer-file-name))
		(:propertize vc-mode face mode-line-highlight)
		" "
		minor-mode-alist
		mode-line-client
		mode-line-remote
		mode-line-frame-identification
		mode-line-end-spaces
		mode-line-misc-info))

(setq blink-cursor-blink -1)

(setq-default display-line-numbers 'relative)
(setq-default display-line-numbers-width nil)
(setq display-raw-bytes-as-hex t)
(setq visible-bell t)

(setq isearch-lazy-count t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'flyspell-mode-hook
	  '(lambda()
	     (define-key flyspell-mode-map (kbd "C-M-i") nil)))
(setq ispell-personal-dictionary (expand-file-name "dictionary" user-emacs-directory))

(setq make-backup-files nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(global-auto-revert-mode t)

(make-directory (expand-file-name "autosave/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "autosave/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))

(setq delete-by-moving-to-trash t)

(setq completion-styles '(initials partial-completion flex basic))
(if (>= emacs-major-version 29)
    (progn
      (icomplete-vertical-mode 1)
      (fido-vertical-mode 1)))

(setq help-window-select t)
(setq switch-to-buffer-obey-display-actions t)
(add-hook 'occur-hook
	  '(lambda ()
	     (switch-to-buffer-other-window "*Occur*")))
;;(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)
(setq display-buffer-alist
      '(("\\*\\(Metahelp\\|Help\\|Apropos\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(shell\\|ansi-term\\|eshell\\|terminal\\|Async Shell\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Messages\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(vc-\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Diff\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Open Recent\\|Ibuffer\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(Embark\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(eldoc\\|xref\\|Flymake\\).*"
	 (display-buffer-in-side-window)
	 (side . top)
	 (window-height . 0.1)
	 (slot . 0))
	("\\*\\(Python\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))
	("\\*\\(log-edit-files\\).*"
	 (display-buffer-no-window))
	("\\*\\(compilation\\|Occur\\|grep\\).*"
	 (display-buffer-in-side-window)
	 (side . bottom)
	 (window-height . 0.4)
	 (slot . 0))))
(global-set-key (kbd "<f10>") 'window-toggle-side-windows)

(scroll-bar-mode -1)

(menu-bar-mode -1)
(setq tty-menu-open-use-tmm t)

(tool-bar-mode -1)

(setq tab-bar-show 1)

(setq use-dialog-box nil)

(tooltip-mode -1)

(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)

(setq tab-always-indent 'complete)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq imenu-auto-rescan t)

(which-function-mode 1)

(setq blink-matching-paren t)
(setq show-paren-style 'mixed)
(setq show-paren-when-point-inside-paren t)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'org-mode-hook 'hs-minor-mode)
(add-hook 'outline-mode-hook 'hs-minor-mode)

(add-hook 'c-mode-hook 'cwarn-mode)

(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)

(setq compilation-auto-jump-to-first-error t)
(setq next-error-highlight 3)
(setq next-error-highlight-no-select 3)
(setq compilation-save-buffers-predicate 'ignore)

(setq grep-save-buffers nil)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(setq flymake-no-changes-timeout nil)

(setq gud-tooltip-echo-area t)

(setq gdb-many-windows t)

(setq initial-scratch-message nil)

(setq vc-follow-symlinks t)
(setq vc-command-messages t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq split-window-function 'split-window-horizontally)
(setq ediff-keep-variants nil)

(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)
(if
    (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(global-set-key (kbd "M-/") 'hippie-expand)

(setq dired-create-destination-dirs 'ask
      dired-dwim-target t)

(setq async-shell-command-display-buffer nil
      shell-command-prompt-show-cwd t)

(setq desktop-restore-frames 1)
(setq desktop-save t)
 (setq desktop-path (list user-emacs-directory))
 (setq desktop-restore-eager 2)
 (savehist-mode 1)
 (setq savehist-file (expand-file-name "savehist" user-emacs-directory))

(setq gnus-init-file "~/etc/emacs/gnus.el")

(setq epg-pinentry-mode 'loopback)

(setq url-configuration-directory user-emacs-directory)
(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-mode-hook 'visual-line-mode)

(require 'org-tempo)

(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))

(load-theme 'modus-vivendi t)

(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

(unless (package-installed-p 'goto-chg)
  (package-refresh-contents)
  (package-install 'goto-chg))

(unless (package-installed-p 'evil-collection)
  (package-refresh-contents)
  (package-install 'evil-collection))

(setq evil-want-C-i-jump t                 ; use C-i for jump list navigation as complement to C-o
      evil-want-C-u-scroll        t        ; C-u scroll up in normal mode
      evil-move-beyond-eol t               ; move one char beyond end of line
      evil-cross-lines        t            ; motions like h, l, f can go to next/prev line
      evil-respect-visual-line-mode t      ; respect visual-line-mode so that j, k move by visual lines
      evil-show-paren-range 1              ; distance from parenthesis to highlight it
      evil-want-fine-undo t                ; use Emacs heuristics for undo
      evil-disable-insert-state-bindings t ; use emacs bindings in insert mode
      evil-want-keybinding nil             ; required for evil-collection
      evil-want-integration t              ; required for evil-collection
      )
					; load evil
(require 'evil)
(evil-mode 1)
(evil-collection-init)

(if (>= emacs-major-version 28)
    (setq evil-undo-system 'undo-redo)          ; native to Emacs 28
  (progn
    (unless (package-installed-p 'undo-tree)
      (package-refresh-contents)
      (package-install 'undo-tree))
    (add-hook 'evil-local-mode-hook 'undo-tree-mode)
    (setq undo-tree-visualizer-diff t
	  undo-tree-visualizer-timestamps t
	  undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo/"))
	  evil-undo-system 'undo-tree)))

(unless (package-installed-p 'marginalia)
  (package-refresh-contents)
  (package-install 'marginalia))
(require 'marginalia)
(setq marginalia-max-relative-age 0)
(marginalia-mode 1)

(unless (package-installed-p 'orderless)
  (package-refresh-contents)
  (package-install 'orderless))
(require 'orderless)

(defun orderless-flex-dispatcher (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun orderless-initials-dispatcher (pattern index _total)
  (when (string-suffix-p "." pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))

(defun orderless-literal-dispatcher (pattern _index _total)
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;; The orderless completion style does not support completion of a
;; common prefix substring, as you may be familiar with from shells or
;; the basic default completion system. The reason is that the
;; Orderless input string is usually not a prefix. In order to support
;; completing prefixes you may want to combine orderless with
;; substring in your completion-styles configuration.

(setq completion-styles '(substring orderless basic))
(setq orderless-matching-styles
      '(orderless-prefixes orderless-initialism orderless-flex orderless-regexp)
      orderless-style-dispatchers
      '(orderless-initials-dispatcher orderless-flex-dispatcher orderless-literal-dispatcher))
(setq completion-category-overrides
      '((file (styles basic partial-completion orderless))
	(buffer (styles basic partial-completion orderless))
	(eglot (styles basic partial-completion orderless))
	(project-file (styles basic substring partial-completion orderless))
	(imenu (styles basic partial-completion orderless))
	(info-menu (styles basic partial-completion orderless))
	))

(if (< emacs-major-version 29)
    (progn
      (unless (package-installed-p 'vertico)
	(package-refresh-contents)
	(package-install 'vertico))
      (require 'vertico)

      (vertico-mode 1)
      (setq vertico-resize t
	    vertico-cycle t)))

;; If you prefer to have the default completion commands a key press
;; away you can add new bindings or even replace the Vertico
;; bindings. Then the default completion commands behave as usual. For
;; example you can use TAB to cycle between candidates if you have
;; set completion-cycle-threshold.
(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "RET") #'minibuffer-force-complete-and-exit)
(define-key vertico-map (kbd "TAB") #'minibuffer-complete)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))
(which-key-mode 1)

(unless (package-installed-p 'embark)
  (package-refresh-contents)
  (package-install 'embark))
(require 'embark)

(unless (package-installed-p 'corfu)
  (package-refresh-contents)
  (package-install 'corfu))
(require 'corfu)

(corfu-global-mode 1)
(setq corfu-preselct-first nil
      corfu-auto t
      corfu-quit-no-match 'separator
      corfu-quit-at-boundary nil
      corfu-preview-current t
      corfu-echo-documentation t
      corfu-cycle t)

(define-key corfu-map "?" #'minibuffer-completion-help)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map (kbd "<tab>") 'corfu-next)
(define-key corfu-map (kbd "<backtab>") 'corfu-previous)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)

(require 'recentf)
(recentf-mode)

(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))
(require 'eglot)

(setq eglot-autoreconnect t
      eglot-send-changes-idle-time 1
      eglot-confirm-server-initiated-edits nil
      eglot-extend-to-xref t)

(add-hook 'python-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (push '(python-mode "~/venv/bin/pylsp" "--verbose") eglot-server-programs))

(add-hook 'c-mode-hook 'eglot-ensure)

(setq eldoc-echo-area-display-truncation-message t
      eldoc-echo-area-use-multiline-p t
      eldoc-echo-area-prefer-doc-buffer nil)

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<SPC>") 'just-one-space) ; restore original mapping for M-SPC
    (define-key map (kbd "i") 'imenu)
    (define-key map (kbd ";") 'comment-line)

    (define-key map (kbd "b b") 'switch-to-buffer)
    (define-key map (kbd "b c") 'clean-buffer-list)
    (define-key map (kbd "b i") 'ibuffer)
    (define-key map (kbd "b k") 'kill-this-buffer)
    (define-key map (kbd "b n") 'next-buffer)
    (define-key map (kbd "b p") 'previous-buffer)
    (define-key map (kbd "b s") 'save-buffer)

    (define-key map (kbd "f f") 'find-file)
    (define-key map (kbd "f o") 'find-file-other-window)
    (define-key map (kbd "f r") 'recentf-open-files)

    (define-key map (kbd "l a") 'eglot-code-actions)
    (define-key map (kbd "l e") 'eglot-events-buffer)
    (define-key map (kbd "l f") 'eglot-format)
    (define-key map (kbd "l r") 'eglot-rename)

    (define-key map (kbd "e a") 'embark-act)
    (define-key map (kbd "e b") 'embark-become)
  map))

(global-set-key (kbd "M-<SPC>") my-mode-map)
