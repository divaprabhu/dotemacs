(setq message-log-max 100000)

(setq inhibit-startup-screen t)

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

(setq minibuffer-completion-auto-choose t) ;; insert current completion candidate in
(setq confirm-nonexistent-file-or-buffer nil) ;; don't ask confirmation

(setq completion-styles '(initials partial-completion flex basic))
(setq completion-auto-help 'lazy)
(setq completion-auto-select nil) 
(setq completion-cycle-threshold 5) ;; always cycle through completion candidates
(setq completions-format 'one-column) ;; completion list buffer format
(setq completions-sort nil) ;; sort candidatate alphabetically
(setq completions-max-height nil) ;; no height limit for completion list buffer
(setq completions-header-format nil) ;; no header in completion list buffer
(define-key minibuffer-local-map (kbd "M-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "M-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
(define-key completion-in-region-mode-map (kbd "M-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "M-n") #'minibuffer-next-completion)
;; M-RET will select the completion. minibuffer-chose-completion

;; (setq minibuffer-eldef-shorten-default t) ; obsolete
(setq minibuffer-default-prompt-format " [%s]")

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
(global-auto-revert-mode 1)

(make-directory (expand-file-name "autosave/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "autosave/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))
(setq delete-auto-save-files t
      kill-buffer-delete-auto-save-files t)

(setq delete-by-moving-to-trash t)

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(require 'recentf)
(recentf-mode 1)			; keybinding in keybindings section toward the end

(setq clean-buffer-list-delay-general 1)

(setq completion-styles '(initials partial-completion flex basic))
(if (>= emacs-major-version 29)
    (progn
      (icomplete-vertical-mode 1)
      (fido-vertical-mode 1)))
(if (>= emacs-major-version 30)
    (progn
      (setq icomplete-in-buffer t)))

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
	("\\*\\(vc-\\|Annotate\\).*"
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

(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)

(setq frame-title-format '(multiple-frames "%b"
	       ("" "%b")))

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

(setq blink-matching-paren 'jump
      blink-matching-delay 1)		; not used in show paren mode

(setq show-paren-highlight-openparen t
      show-paren-style 'mixed
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen t)
(show-paren-mode 1)

(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs t)

(add-hook 'prog-mode-hook 'electric-pair-local-mode)

(setq eldoc-echo-area-display-truncation-message t
      eldoc-idle-delay 0.5
      eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-prefer-doc-buffer t)
(global-eldoc-mode 1)

(setq hs-isearch-open t
      hs-hide-comments-when-hiding-all t)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(if (>= emacs-major-version 30)
  (progn
    (require 'completion-preview)
    (add-hook 'prog-mode-hook #'completion-preview-mode)
    (add-hook 'text-mode-hook #'completion-preview-mode)
    (add-hook 'comint-mode-hook #'completion-preview-mode)
    ;; Show the preview already after two symbol characters
    (setq completion-preview-minimum-symbol-length 2)
    ;; Org mode has a custom `self-insert-command'
    (push 'org-self-insert-command completion-preview-commands)
    ;; Cycle the completion candidate that the preview shows
    (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
    (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
    ;; Convenient alternative to C-i or TAB after typing one of the above
    (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert)))

(add-hook 'prog-mode-hook 'superword-mode)

(add-hook 'c-mode-hook 'cwarn-mode)

(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)

(setq compilation-auto-jump-to-first-error t)
(setq next-error-highlight 3)
(setq next-error-highlight-no-select 3)
(setq compilation-save-buffers-predicate 'ignore)

(setq grep-save-buffers nil
      grep-use-null-filename-separator nil)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(setq flymake-no-changes-timeout 3
      flymake-start-on-flymake-mode t
      flymake-wrap-around t
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 1)
(add-hook 'prog-mode-hook 'flymake-mode)

(setq gud-tooltip-echo-area t)

(setq gdb-many-windows t)

(defun my/ielm-send-line-or-region ()
  (interactive)
  (unless (use-region-p)
    (forward-line 0)
    (set-mark-command nil)
    (forward-line 1))
  (backward-char 1)
  (let ((text (buffer-substring-no-properties (region-beginning)
					      (region-end))))
    (with-current-buffer "*ielm*"
      (insert text)
      (ielm-send-input))

    (deactivate-mark)))

(defun my/show-ielm ()
  (interactive)
  (select-window (split-window-vertically -10))
  (ielm)
  (text-scale-set 1))

(define-key org-mode-map (kbd "C-M-x") 'my/ielm-send-line-or-region)

(setq vc-revert-show-diff t)

(setq vc-follow-symlinks t)
(setq vc-command-messages t)

(setq read-file-name-completion-ignore-case t
      xref-search-program-alist '((grep . "xargs -0 grep <C> -snHE -e <R>")))

(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)
(if
    (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(abbrev-mode -1)			; don't expand automatically on space or punctuation
(setq abbrev-suggest t)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq async-shell-command-display-buffer nil
      async-shell-command-buffer 'new-buffer
      shell-command-prompt-show-cwd t)

(setq desktop-restore-eager 2
      desktop-lazy-idle-delay 2
      desktop-load-locked-desktop 'ask
      desktop-restore-frames 1
      desktop-save t
      desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 60
      desktop-base-file-name "emacs.desktop"
      desktop-globals-to-save
      '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
      desktop-locals-to-save
      '(buffer-undo-list eww-history-position desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system buffer-display-time indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace))
(desktop-save-mode t)

(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1)

(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-forget-unreadable-files t) ; set to nil if emacs is slow to exit

(use-package modus-themes
  :defer nil
  :custom
  (modus-themes-inhibit-reload nil "reload active theme when an option is changed through the Customize UI")
  (modus-themes-bold-constructs t  "Use bold for code syntax highlighting and related")
  (modus-themes-italic-constructs t "Use bold for code syntax highlighting and related ")
  (modus-themes-mode-line '(accented borderless (padding 4) (height 0.9)) "Model")
  (modus-themes-hl-line '(intense) "amplify color in use for hl-line-mode heading sizes and colors")
  :config
  (setq modus-themes-headings '((1 . (bold rainbow 1.5))
			  (2 . (bold rainbow 1.4))
			  (3 . (bold rainbow 1.2))
			  (t . (monochrome ))))
  (load-theme 'modus-vivendi t))

(use-package eglot
  :custom
  (eglot-autoreconnect t "Automatically reconnect to LSP server")
  (eglot-connect-timeout 30 "Time out connection attempt after specified seconds")
  (eglot-sync-connect nil "Don't block Emacs user interface when connecting")
  (eglot-events-buffer-size 200000000 "Max number of chars on event buffer")
  (eglot-autoshutdown t "Shutdown language server when last buffer managed by it is killed")
  (eglot-confirm-server-initiated-edits nil "don't confirm server initiated edits with user")
  (eglot-ignored-server-capabilities nil "LSP capabilities that should not be used")
  (eglot-extend-to-xref t "activate eglot in non-project cross-referenced files")
  (eglot-send-changes-idle-time 1 "Send changes to LSP server after so many idle seconds")
  (eglot-report-progress nil "Don't spam echo area")
  :bind
  (:map eglot-mode-map
	("C-c l a" . eglot-code-actions)
	("C-c l b e" . eglot-events-buffer)
	("C-c l b s" . eglot-stderr-buffer)
	("C-c l f" . eglot-format)
	("C-c l l" . eglot)
	("C-c l r" . eglot-rename)
	("C-c l s" . eglot-shutdown-all))
  :config
  (if (eq system-type 'windows-nt)
      (setq exec-path (append exec-path '("~/.cache/emacs/lsp/pylsp/Scripts")))
    (setq exec-path (append exec-path '("~/.cache/emacs/lsp/pylsp/bin")))))
;;   :config
;;
;; (add-to-list 'eglot-server-programs '(python-base-mode . ("~/.cache/emacs/lsp/pylsp/bin/pylsp" "--verbose"))))

;; (add-to-list 'eglot-server-programs '(python-base-mode . ("~/.cache/emacs/lsp/basedpyright/bin/basedpyright-langserver" "--stdio"))))

(use-package pyvenv
  :ensure t
  ;; :vc (:url "https://github.com/jorgenschaefer/pyvenv") 
  :hook
  (python-base-mode)
  :config
  (setenv "WORKON_HOME" "~/.cache/venvs")
  (pyvenv-tracking-mode 1))

(use-package python
  :init
  (let ((pylspdir (expand-file-name "lsp/pylsp" user-emacs-directory)))
    (unless (file-directory-p pylspdir)
      (make-directory pylspdir t)
      (cond
       ((eq system-type 'windows-nt)
	(shell-command (concat "python -m venv " pylspdir))
	(shell-command (concat pylspdir "/Scripts/activate.bat && pip install -U pip python-lsp-server[all]")))
       (t
	(shell-command (concat "python3 -m venv " pylspdir))
	(shell-command (concat ". " pylspdir "/bin/activate && pip install -U pip python-lsp-server[all]"))))))
  :config
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  :bind
  (:map python-mode-map
	("C-c C-c"	. python-shell-send-buffer)
	("C-c C-e"	. python-shell-send-statement)
	("C-c C-r"	. python-shell-send-region)
	("C-c C-p"	. run-python)
	("C-c C-z"	. python-shell-switch-to-shell)
	("C-c C-t c"	. python-skeleton-class)
	("C-c C-t d"	. python-skeleton-def)
	("C-c C-t f"	. python-skeleton-for)
	("C-c C-t i"	. python-skeleton-if)
	("C-c C-t t"	. python-skeleton-import)
	("C-c C-t w"	. python-skeleton-while)))

(use-package window
  :config
  (repeat-mode 1)
  :bind
  ("M-o" . other-window)
  ("C-c t t" . term)
  ("C-c t s" . shell)
  ("C-c t e" . eshell)
  (:repeat-map my/window-repeat-map
	       ;; Defaults:
	       ("o" . other-window)	; enters the map here
	       ;; Resizing:
	       ("L" . enlarge-window-horizontally)
	       ("H" . shrink-window-horizontally)
	       ("=" . balance-windows)
	       ;; Adding/Deleting:
	       ("0" . delete-window)
	       ("1" . delete-other-windows)))

(use-package simple
  :config
  (repeat-mode 1)
  :bind
  ("C-x C-t" . transpose-lines)
  ("C-x x a" . append-to-buffer)
  ("C-x x p" . prepend-to-buffer)
  ("C-x x c" . copy-to-buffer)
  ("C-x x i" . insert-buffer)
  ("C-x x f" . append-to-file)
  (:repeat-map my/simple-repeat-map
	       ;; Defaults:
	       ("C-t" . transpose-lines)))

(use-package org
  :config
  (repeat-mode 1)
  :bind
  (:map org-mode-map
	("C-c C-n" . org-next-visible-heading)
	("C-c C-p" . org-previous-visible-heading)
	("C-c C-f" . org-forward-heading-same-level)
	("C-c C-b" . org-backward-heading-same-level)
	("C-c C-u" . outline-up-heading))
  (:repeat-map my/org-repeat-map
	       ("C-n" . org-next-visible-heading)
	       ("C-p" . org-previous-visible-heading)
	       ("C-f" . org-forward-heading-same-level)
	       ("C-b" . org-backward-heading-same-level)
	       ("C-u" . outline-up-heading)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package ibuffer
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-movement-cycle nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-formats
   '((mark modified read-only locked " "
	   (name 40 40 :left :elide)
	   " "
	   (size 9 -1 :right)
	   " "
	   (mode 16 16 :left :elide)
	   " " filename-and-process)
     (mark " "
	   (name 16 -1)
	   " " filename)))
  (ibuffer-saved-filter-groups nil)
  (ibuffer-old-time 48)
  :bind
  (
   (:map ibuffer-mode-map
	 ("* f" . ibuffer-mark-by-file-name-regexp)
	 ("* g" . ibuffer-mark-by-content-regexp)
	 ("* n" . ibuffer-mark-by-name-regexp)
	 ("s n" . ibuffer-do-sort-by-alphabetic)
	 ("/ g" . ibuffer-filter-by-content)
	 ("M-o" . other-window))
   (:map ctl-x-map
	 ("C-b" . ibuffer-jump))))

(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 3)
  (which-key-side-window-max-height 0.5)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(setq doc-view-resolution 300)
