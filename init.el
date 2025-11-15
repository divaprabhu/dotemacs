(use-package emacs			; basic configs
  :init
  (repeat-mode 1)
  :custom
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory))
  (transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))
  (url-configuration-directory (expand-file-name "cache/url/" user-emacs-directory))
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))

  :config
  (define-prefix-command 'my/global-prefix-map nil)
  (keymap-set global-map "M-<SPC>" my/global-prefix-map)

  (setq line-move-visual nil	 ; C-n C-p move by screen-lines
	track-eol t		 ; don't track end of line when moving
	next-line-add-newline nil ; C-n at the end of buffer won't add new lines
	what-cursor-show-names t ; show Unicode char name in what-cursor-position
	delete-by-moving-to-trash t ; delete from dired moves to trash

	;; font
	hi-lock-auto-select-face t	; don't prompt face

	;; indentation
	tab-always-indent 'complete ; tab indents if possible else completes
	tab-width 8		    ; default tab width
	repeat-exit-timeout 5 ; idle seconds after which turn of repeat mode
	completion-ignore-case t	; case insensitive completion
	native-comp-async-on-battery-power nil ; Do not native compile if on battery power
	truncate-lines t	      ; truncate display of long lines
	pixel-scroll-precision-mode t ; smooth scrolling
	pixel-scroll-precision-use-momentum nil ; stop scrolling when wheel movement stops

	)

  ;; custom variable file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))

  ;; A Protesilaos life savier HACK
  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff (if you choose `d') of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
	       (list "d"
		     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
		     "show diff between the buffer and its file"))

  (setq-default indicate-empty-lines t ; show blank lines at the end of buffer
		)
  :bind
  (:repeat-map my/buffer-repeat-map
	       ("C-t" . transpose-lines)
	       )

  ("C-x x a" . append-to-buffer)
  ("C-x x p" . prepend-to-buffer)
  ("C-x x c" . copy-to-buffer)
  ("C-x x i" . insert-buffer)
  ("C-x x f" . append-to-file)
  ("C-x C-k RET" . nil)			; disable kmacro edit
  ("C-x z" . nil)			; disable suspend frame
  ("C-x C-x" . nil)			; disable suspend frame
  (:map my/global-prefix-map
	("M-SPC" . cycle-spacing)	; restore original binding
	("c j" . duplicate-dwim)	; duplicate line or region
	("c ;" . comment-line)		; comment line
	)
  :hook
  (text-mode . turn-on-auto-fill)   ; automatic line breaking on space
  (prog-mode . superword-mode) ; treat underscore as word char for navigation
  (c-mode . cwarn-mode)

  )
(use-package emacs			; mark and kill
  :config
  (setq delete-active-region 'kill     ; kill region instead of delete
	mark-even-if-inactive nil ; don't use mark if region is inactive
	set-mark-command-repeat-pop t ; C-u C-SPC C-SPC... keeps popping local mark
	mark-ring-max 512	      ; local mark ring size
	global-mark-ring-max 512      ; global mark ring size
	kill-do-not-save-duplicates t ; don't save duplicates in kill ring
	kill-whole-line t ; kill-line at line start deletes newline also
	kill-read-only-ok t ; no error in read-only buffer, add to clipboard
	kill-ring-max 1000  ; kill ring size
	save-interprogram-paste-before-kill t ; save clipboard to kill ring before replacing it with kill
	use-empty-active-region nil ; region aware commands treat empty region as inactive
	highlight-nonselected-windows t ; each window highlights its own region
	)
  (delete-selection-mode 1)    ; typing with region active replaces it
  (defun my/kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
	(kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))
  (global-set-key (kbd "C-w") 'my/kill-region-or-backward-word)

  )
(use-package emacs			; letter case
  :bind
  ;; change case commands map to dwim variant which apply to region if selected
  ([M-d] . 'downcase-dwim)
  ([M-u] . 'upcase-dwim)
  ([M-c] . 'capitalize-dwim)
  :config
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  )
(use-package emacs			; backup and save
  :config
  (setq make-backup-files nil ; don't create file backups
	backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))) ; backup directory
	auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory) ; auto-save directory
	auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t))
	delete-auto-save-files t ; delete on buffer save
	clean-buffer-list-delay-general 1 ; auto kill buffer after 1 day
	)
  (make-directory (expand-file-name "autosave/" user-emacs-directory) t)
  )
(use-package emacs			; autorevert
  :config
  (setq	auto-revert-verbose nil	       ; don't flash echo area message
	global-auto-revert-non-file-buffers t ; auto revert for dired buffers etc
	auto-revert-remote-files nil	      ; disable for tramp
	)
  (global-auto-revert-mode 1)	 ; auto update buffers if file changes

  )
(use-package minibuffer
  :config
  (setq minibuffer-follows-selected-frame nil ; minibuffer stays in same frame
	insert-default-directory t ; start with default directory in minibuffer
	max-mini-window-height 0.25 ; default value, 25% of frame height
	resize-mini-windows t ; resize mini-buffer based on text in it
	enable-recursive-minibuffer t ; allow to use mini-buffer recursively
	minibuffer-depth-indicate-mode t ; show depth in case of recursion
	minibuffer-electric-default-mode t
	confirm-nonexistent-file-or-buffer nil ; don't ask confirmation
	use-short-answers t	     ; use y or n instead of yes or no

	;; mini-buffer completion
	minibuffer-completion-auto-choose t ; insert current completion candidate in mini-buffer
	completion-styles '(partial-completion flex initials) ; completion styles
	completion-auto-help 'lazy ; show completion buffer if can't complete
	completion-auto-select 'second-tab ; select completion buffer on second tab
	completion-show-help nil	   ; no help in completion buffer
	completion-eager-update t	   ;
	completion-ignore-case t	   ; case insensitive completion
	completion-cycle-threshold 10 ; always cycle if number of completions is less than this number
	completions-format 'one-column ; completion list buffer format
	completions-sort 'historical ; sort alphabetically and then by history
	completions-max-height 10 ; height limit for completion list buffer
	completions-header-format nil ; no header in completion list buffer
	completions-detailed t	      ; display completions with details. Useful in describe-function etc
	read-buffer-completion-ignore-case t ; ignore case for buffer name completion
	read-file-name-completion-ignore-case t ;ignore case for file name completion
	minibuffer-default-prompt-format " [%s]" ; format string for default values

	;; mini-buffer history
	history-length 1000		; minibuffer history length
	history-delete-duplicates t	; remove duplicates
	savehist-file (expand-file-name "savehist" user-emacs-directory) ; location of minibuffer history file
	savehist-minibuffer-history-variables '(minibuffer-history
						query-replace-history
						file-name-history
						buffer-name-history
						regexp-history
						extended-command-history
						shell-command-history
						read-expression-history
						command-history)
	savehist-additional-variables '(kill-ring      ; clipboard
					register-alist ; macros
					mark-ring global-mark-ring ; marks
					search-ring regexp-search-ring) ; searches
	)

  ;; mini-buffer
  (line-number-mode 1)	    ; show line number in mode-line
  (column-number-mode 1)    ; show column number in mode-line
  (global-hl-line-mode 1)   ; highlight the line the point is on
  (size-indication-mode -1) ; disable buffer size display in mode-line
  (file-name-shadow-mode 1) ; shadow ignored file path in mini-buffer

  ;; history
  (savehist-mode 1)			; save minibuffer history
  )
(use-package icomplete
  :bind (:map icomplete-minibuffer-map
	      ("C-n" . icomplete-forward-completions)
	      ("C-p" . icomplete-backward-completions)
	      ("RET" . icomplete-force-complete-and-exit)
	      ("C-j" . exit-minibuffer)) ;; So we can exit commands like `multi-file-replace-regexp-as-diff'
  :hook
  (after-init-hook . (lambda ()
		       (fido-mode -1)
		       (icomplete-vertical-mode 1)))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)

  (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
  )
(use-package completion-preview
  :hook
  (prog-mode . 'completion-preview)
  (text-mode . 'completion-preview)
  (comint-mode . 'completion-preview)
  :config
  (setq completion-preview-minimum-symbol-length 1) ; minimum number of chars to start completion
  (push 'org-self-insert-command completion-preview-commands)
  :bind
  (:map completion-preview-active-mode-map
	("M-n" . completion-preview-next-candidate)
	("M-p" . completion-preview-prev-candidate)
	("TAB" . completion-preview-complete)
	("M-i" . completion-preview-insert))
  )
(use-package recentf
  :custom
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  :config
  (recentf-mode 1)
  )
(use-package register
  :custom
  (register-use-preview t)
  :config
  (setq register-preview-delay 1) ; seconds before displaying preview of register list
  )
(use-package bookmark
  :config
  (setq bookmark-save-flag 1)	 ; auto save bookmarks to file
  )
(use-package isearch
  :config
  (setq	search-ring-max 1000 ; search ring size
	search-exit-option t ; control chars end search
	isearch-allow-scroll 'unlimited ; allow screen scroll when in isearch
	regexp-search-ring-max 1000    ; regex search ring size
	search-default-mode t	       ; default regex search
	isearch-lazy-count t)	       ; show current match and total match number
  )
(use-package window
  :defer nil
  :config
  (setq	help-window-select t	  ; switch to help window when created
	help-window-keep-selected t	; reuse same Help buffer
	next-screen-context-lines 3 ; number lines that overlap during scroll command
	scroll-conservatively 1000 ; don't recentre point during long jump
	hscroll-margin 5 ; horizontally scroll long lines near the edge
	hscroll-step 5	 ; horizontally scroll only by small amount
	line-number-display-limit nil ; no size limit to display line numbers in mode line
	blink-cursor-blink -1	      ; don't stop cursor blink
	display-line-numbers 'relative	; relative line numbers
	display-line-numbers-width nil ; dynamically compute line number width
	display-line-numbers-widen t ; show actual line number in narrow
	visible-bell t		       ; don't beep but flash
	switch-to-buffer-in-dedicated-window 'pop ; in strongly dedicate windows behave like pop-to-buffer
	switch-to-buffer-obey-display-actions t	; C-x C-b respects display buffer rules
	horizontal-scroll-bar-mode -1 ; don't show horizontal scroll bar
	tty-menu-open-use-tmm t	    ; f10 invokes menu bar in terminal
	use-dialog-box nil    ; don't show dialog box, use mini-buffer
	frame-title-format '(multiple-frames "%b" ; show buffer name
					     ("" "%b"))
	display-buffer-alist
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
	   (side . top)
	   (window-height . 0.2)
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

  (put 'scroll-left 'disabled nil)	; allow scrolling left
  (put 'narrow-to-region 'disabled nil) ; allow region narrowing
  (put 'narrow-to-page 'disabled nil)	; allow narrow to page
  (global-display-line-numbers-mode 1) ; display line numbers in the fringe
  (tooltip-mode -1)		       ; tooltip in echo area
  (winner-mode)
  (defun my/maximize-frame ()
    "Maximizes the active frame in Windows"
    (interactive)
    ;; Send a `WM_SYSCOMMAND' message to the active frame with the
    ;; `SC_MAXIMIZE' parameter.
    (when (eq system-type 'windows-nt)
      (w32-send-sys-command 61488))
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))
  (add-hook 'window-setup-hook 'my/maximize-frame t)

  :bind
  ("M-o" . other-window)
  (:map my/global-prefix-map
	("w o" . other-window)
	("w }" . enlarge-window-horizontally)
	("w {" . shrink-window-horizontally)
	("w 0" . delete-window)
	("w 1" . delete-other-windows)
	("w =" . balance-windows)
	("w t" . window-toggle-side-windows)
	("w r" . winner-redo)
	("w u" . winner-undo))
  (:repeat-map my/window-repeat-map
	       ("o" . other-window)
	       ("}" . enlarge-window-horizontally)
	       ("{" . shrink-window-horizontally)
	       ("0" . delete-window)
	       ("1" . delete-other-windows)
	       ("=" . balance-windows)
	       ("t" . window-toggle-side-windows)
	       ("r" . winner-redo)
	       ("u" . winner-undo))
  )
(use-package flyspell
  :defer t
  :config
  (setq ispell-personal-dictionary (expand-file-name "dictionary" user-emacs-directory) ; location of personal
	)

  :hook
  (text-mode-hook . flyspell-mode)	; fly-spell in text mode
  (prog-mode-hook . flyspell-prog-mode) ; fly-spell in progmode comment
  )
(use-package kmacro
  :defer t
  :config
  (setq	kmacro-ring-max 1000) ; macro ring size
  :config
  (if (file-exists-p (expand-file-name "macros" user-emacs-directory))
      (load-file (expand-file-name "macros" user-emacs-directory)))
  )
(use-package tramp
  :custom
  (tramp-copy-size-limit (* 2 1024 1024)) ;; 2MB
  (tramp-use-scp-direct-remote-copying t)
  (tramp-verbose 2)
  :defer t
  :config
  ;; use $PATH from after .profile load in executable search path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(use-package image
  :defer t
  :config
  (setq image-use-external-converter t) ; use imagemagick for unreadable images
  )
(use-package occur
  :hook
  (occur-mode . next-error-follow-minor-mode)	; auto enable follow mode
  (occur-mode . (lambda() (switch-to-buffer-other-window "*Occur*")))
  )
(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error ; scroll automatically
	compilation-auto-jump-to-first-error t ; jump to first error
	next-error-highlight 3		       ; highlight error for 3 sec in source
	next-error-highlight-no-select 3       ; highlight in non selected buffers in source
	compilation-save-buffers-predicate 'ignore ; don't save
	compilation-always-kill t) ; kill current compilation before starting new one

  (add-hook 'compilation-finish-functions ; switch to compile buffer immediately
	    'switch-to-buffer-other-window 'compilation)
  )
(use-package org
  :defer t
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
	       ("C-u" . outline-up-heading))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((C . t)
				 (java . t)
				 (latex . t)
				 (lua . t)
				 (python . t)
				 (shell . t)
				 (emacs-lisp . t)))

  (setq org-confirm-babel-evaluate nil)	; don't ask when evaluating code blocks

  )
(use-package imenu
  :config
  (setq imenu-auto-rescan t		; rescan buffer automatically
	)
  )
(use-package emacs			;electric pair
  :config
  (setq blink-matching-paren 'jump	; briefly move to matching open paren
	blink-matching-delay 1		; not used in show paren mode
	show-paren-highlight-openparen t ; highlight open paren when point is just before it
	show-paren-delay 0		 ; time in sec before showing matching paren
	show-paren-style 'mixed		 ; highlight both paren when visible else highlight expression in between
	show-paren-when-point-inside-paren t ; highlight when point is inside paren
	show-paren-context-when-offscreen t  ; show some context in echo when open paren is offscreen
	electric-pair-preserve-balance t     ; balance paren
	electric-pair-delete-adjacent-pairs t ; backspace of open paren also deletes close paren when both are nearby
	electric-pair-open-newline-between-pairs t) ; newline between adjacent parens open new one

  (show-paren-mode 1)

  :hook
  (prog-mode . electric-pair-local-mode)
  )
(use-package eldoc
  :defer t
  :custom
  (eldoc-help-at-pt t) ;; EMACS-31
  :config
  (setq eldoc-echo-area-display-truncation-message t ; indicate if message was truncated
	eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly ; show multiple documentation as soon as they are available
	eldoc-idle-delay 0.5	; wait before displaying documentation
	eldoc-echo-area-use-multiline-p t ; don't allow multilne docs in echo area
	eldoc-echo-area-prefer-doc-buffer t) ; reuse existing eldoc buffer

  (global-eldoc-mode 1)			; enable eldoc mode
  )
(use-package hideshow
  :config
  (setq hs-isearch-open t ; unhide code and comment if match is in hidden block during isearch
	hs-hide-comments-when-hiding-all t) ; hide comments also when hs-hide-all
  :bind
  (:map my/global-prefix-map
	("h h" . hs-toggle-hiding)
	("h H" . hs-hide-all)
	("h S" . hs-show-all))
  (:repeat-map my/hs-minor-repeat-map
	       ("t" . hs-toggle-hiding)
	       ("H" . hs-hide-all)
	       ("S" . hs-show-all))
  :hook
  (prog-mode . hs-minor-mode)
  )
(use-package grep
  :defer t
  :custom
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".venv" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  :config
  (setq grep-save-buffers 'ask		; ask to save buffer
	grep-use-null-filename-separator nil) ; don't use --null option of grep
  )
(use-package flymake
  :defer t
  :bind (:map flymake-mode-map
	      ("M-n" . 'flymake-goto-next-error)
	      ("M-p" . 'flymake-goto-prev-error))
  :config
  (setq flymake-no-changes-timeout 3	  ; wait 3 sec before checking
	flymake-show-diagnostics-at-end-of-line nil ; add diagnostic summary at end of line
	flymake-start-on-flymake-mode t ; start checking when enabled
	flymake-wrap-around t		; wrap around
	help-at-pt-display-when-idle t ; show local help on point over
	help-at-pt-timer-delay 1       ; show help after 1 sec
	)

  (remove-hook 'lisp-interaction-mode-hook 'flymake-mode)
  :hook
  (prog-mode . flymake-mode)
  )
(use-package gud
  :defer t
  :config
  (setq gud-tooltip-echo-area t	       ; display tool tip in echo area
	gdb-many-windows t)		       ; enable gdb many window mode

  )
(use-package elisp-mode
  :after org
  :defer t
  :config
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
  (define-key org-mode-map (kbd "C-M-x") 'my/ielm-send-line-or-region)
  )
(use-package vc
  :defer t
  :config
  (setq vc-revert-show-diff t	      ; revert first shows diff buffer
	vc-follow-symlinks t	      ; follow symlinks
	vc-command-messages t	      ; log backend commands being run
	)
  (defun my/vc-git-clone (repository-url local-dir)
    "Run \"git clone REPOSITORY-URL\" to LOCAL-DIR."
    (interactive
     (let* ((url (read-string "Repository URL: "))
	    (dir (file-name-base url)))
       (list url (read-string "Target directory: " dir))))
    (vc-git-command nil 0 nil "clone" repository-url local-dir)
    (let ((default-directory (file-name-concat default-directory local-dir)))

      (vc-dir default-directory)))
  )
(use-package xref
  :defer t
  :config
  (setq xref-search-program-alist '((grep . "xargs -0 grep <C> -snHE -e <R>")) ; argument to xref-search-program
	)
  )
(use-package abbrev
  :defer t
  :bind
  ("M-/" . 'hippie-expand)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory) ; location to store personal abbrevs
	save-abbrevs 'silently		; save abbrev when file is saved
	abbrev-suggest t)		; suggest using abbrev

  (if
      (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (abbrev-mode -1)			; don't expand automatically on space or punctuation
  )
(use-package shell
  :defer t
  :config
  (setq async-shell-command-display-buffer nil ; display command buffer after command completion
	async-shell-command-buffer 'new-buffer ; create new buffer if there is already a buffer from another command
	shell-command-prompt-show-cwd t)       ; show current dir in shell-command and async-shell-command
  )
(use-package desktop
  :demand t
  :init
  (setq desktop-restore-eager 2		; number of buffers to restore eagerly
	desktop-lazy-idle-delay 2	; idle delay for creating other buffers lazily
	desktop-load-locked-desktop 'ask ; notify if another emacs instance is locking session
	desktop-restore-frames 1	 ; save and restore frames and window config
	desktop-save t			 ; always save desktop when quitting emacs
	desktop-path (list user-emacs-directory) ; list of directories to search for desktop file
	desktop-auto-save-timeout 60	; idle time seconds before autosaving
	desktop-base-file-name "emacs.desktop" ; desktop file name
	desktop-dirname (expand-file-name user-emacs-directory)
	desktop-globals-to-save		       ; global variables to be saved
	'(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
	desktop-locals-to-save		; local variables to be saved
	'(buffer-undo-list eww-history-position desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system buffer-display-time indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace))
  :config
  (desktop-save-mode t)			; save desktop

  )
(use-package saveplace
  :demand t
  :custom
  (save-place-limit 600)
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory) ; file where place is stored
	save-place-forget-unreadable-files t) ; set to nil if emacs is slow to exit

  (save-place-mode 1)			; enable saveplace mode
  )
(use-package dired
  :defer t
  :custom
  (dired-dwim-target t)			; try to guess target directory
  (dired-kill-when-opening-new-dired-buffer t) ; kill current buffer when opening new directoy
  (dired-listing-switches "-alh")
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  (image-dired-dir (expand-file-name "cache/image-dired" user-emacs-directory))
  )
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))
(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-headings
   '((1 . (variable-pitch 1.7))
     (2 . (1.5))
     (3 . (1.3))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.1))))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((accent-0 "#a1bfff")
     (accent-1 "#79a8ff")
     (bg-active bg-main)
     (bg-added "#2A3B2E")
     (bg-added-refine "#384c3f")
     (bg-changed "#3C435E")
     (bg-changed-refine "#4F5875")
     (bg-completion "#2f447f")
     (bg-completion-match-0 bg-main)
     (bg-completion-match-1 bg-main)
     (bg-completion-match-2 bg-main)
     (bg-completion-match-3 bg-main)
     (bg-hl-line "#30344a")
     (bg-hover-secondary "#676E95")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#292D3E")
     (bg-main "#292D3E")
     (bg-mark-delete "#4d2d2d")
     (bg-mark-select "#3C435E")
     (bg-mode-line-active "#181818") ;"#232635")
     (bg-mode-line-inactive "#424242") ;"#282c3d")
     (bg-prominent-err "#4d2d2d")
     (bg-prompt unspecified)
     (bg-prose-block-contents "#232635")
     (bg-prose-block-delimiter bg-prose-block-contents)
     (bg-region "#3C435E")
     (bg-removed "#4d2d2d")
     (bg-removed-refine "#603939")
     (bg-tab-bar      "#292D3E")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#292D3E")
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (builtin "#82aaff")
     (comment "#676E95")
     (constant  "#f78c6c")
     (cursor  "#EEFFFF")
     (date-weekday "#82aaff")
     (date-weekend "#f78c6c")
     (docstring "#8d92af")
     (err     "#ff5370")
     (fg-active fg-main)
     (fg-completion "white")
     (fg-completion-match-0 "#82aaff")
     (fg-completion-match-1 "#ff5370")
     (fg-completion-match-2 "#c3e88d")
     (fg-completion-match-3 "#f78c6c")
     (fg-heading-0 "#82aaff")
     (fg-heading-1 "#82aaff")
     (fg-heading-2 "#c792ea")
     (fg-heading-3 "#bb80b3")
     (fg-heading-4 "#a1bfff")
     (fg-line-number-active fg-main)
     (fg-line-number-inactive "gray50")
     (fg-link  "#82aaff")
     (fg-main "#EEFFFF")
     (fg-mark-delete "#ff5370")
     (fg-mark-select "#82aaff")
     (fg-mode-line-active "#A6Accd")
     (fg-mode-line-inactive "#676E95")
     (fg-prominent-err "#ff5370")
     (fg-prompt "#c792ea")
     (fg-prose-block-delimiter "#676E95")
     (fg-prose-verbatim "#c3e88d")
     (fg-region "white")
     (fnname    "#82aaff")
     (fringe "#292D3E")
     (identifier "#c792ea")
     (info    "#89DDFF")
     (keyword   "#89DDFF")
     (name "#82aaff")
     (number "#f78c6c")
     (property "#82aaff")
     (string "#c3e88d")
     (type      "#c792ea")
     (variable  "#c792ea")
     (warning "#ffcb6b")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(change-log-acknowledgment ((,c :foreground "#a1bfff")))
     `(change-log-date ((,c :foreground "#c3e88d")))
     `(change-log-name ((,c :foreground "#f78c6c")))
     `(diff-context ((,c :foreground "#82aaff")))
     `(diff-file-header ((,c :foreground "#bb80b3")))
     `(diff-header ((,c :foreground "#82aaff")))
     `(diff-hunk-header ((,c :foreground "#f78c6c")))
     `(gnus-button ((,c :foreground "#82aaff")))
     `(gnus-group-mail-3 ((,c :foreground "#82aaff")))
     `(gnus-group-mail-3-empty ((,c :foreground "#82aaff")))
     `(gnus-header-content ((,c :foreground "#89DDFF")))
     `(gnus-header-from ((,c :foreground "#c792ea")))
     `(gnus-header-name ((,c :foreground "#c3e88d")))
     `(gnus-header-subject ((,c :foreground "#82aaff")))
     `(log-view-message ((,c :foreground "#a1bfff")))
     `(match ((,c :background "#3C435E" :foreground "#EEFFFF")))
     `(modus-themes-search-current ((,c :background "#ff5370" :foreground "#292D3E" )))
     `(modus-themes-search-lazy ((,c :background "#3C435E" :foreground "#EEFFFF")))
     `(newsticker-extra-face ((,c :foreground "#8d92af" :height 0.8 :slant italic)))
     `(newsticker-feed-face ((,c :foreground "#ff5370" :height 1.2 :weight bold)))
     `(newsticker-treeview-face ((,c :foreground "#EEFFFF")))
     `(newsticker-treeview-selection-face ((,c :background "#3C435E" :foreground "#EEFFFF")))
     `(tab-bar ((,c :background "#292D3E" :foreground "#A6Accd")))
     `(tab-bar-tab ((,c :background "#292D3E" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#292D3E" :foreground "#A6Accd" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#292D3E" :foreground "#777")))
     `(tab-bar-tab-inactive ((,c :background "#292D3E" :foreground "#676E95")))
     `(vc-dir-file ((,c :foreground "#82aaff")))
     `(vc-dir-header-value ((,c :foreground "#a1bfff")))))
  :init
  (load-theme 'modus-vivendi-tinted t))
(use-package ibuffer
  :custom
  (ibuffer-expert t)	      ; don't confirm for dangerous operations
  (ibuffer-display-summary nil)	     ; don't summarize ibuffer columns
  (ibuffer-show-empty-filter-groups nil) ; don't show empty filter groups
  (ibuffer-default-sorting-mode 'major-mode) ; sort order
  (ibuffer-use-header-line t)		     ; show header line
  (ibuffer-default-shrink-to-minimum-size nil) ; don't minimize window size
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
  (ibuffer-saved-filter-groups nil)	; no defined filter by default
  (ibuffer-old-time 48)	  ; hours after which buffer is considered old
  (ibuffer-human-readable-size t)	; human readable size
  :config
  ;; Ibuffer filters
  (setq ibuffer-saved-filter-groups
	'(("default"
	   ("org"     (or
		       (mode . org-mode)
		       (name . "^\\*Org Src")
		       (name . "^\\*Org Agenda\\*$")))
	   ("tramp"   (name . "^\\*tramp.*"))
	   ("emacs"   (or
		       (name . "^\\*scratch\\*$")
		       (name . "^\\*Messages\\*$")
		       (name . "^\\*Warnings\\*$")
		       (name . "^\\*Shell Command Output\\*$")
		       (name . "^\\*Async-native-compile-log\\*$")))
	   ("ediff"   (name . "^\\*[Ee]diff.*"))
	   ("vc"      (name . "^\\*vc-.*"))
	   ("dired"   (mode . dired-mode))
	   ("terminal" (or
			(mode . term-mode)
			(mode . shell-mode)
			(mode . eshell-mode)))
	   ("help"    (or
		       (name . "^\\*Help\\*$")
		       (name . "^\\*info\\*$")))
	   ("news"    (name . "^\\*Newsticker.*"))
	   ("gnus"    (or
		       (mode . message-mode)
		       (mode . gnus-group-mode)
		       (mode . gnus-summary-mode)
		       (mode . gnus-article-mode)
		       (name . "^\\*Group\\*")
		       (name . "^\\*Summary\\*")
		       (name . "^\\*Article\\*")
		       (name . "^\\*BBDB\\*")))
	   ("chat"    (or
		       (mode . rcirc-mode)
		       (mode . erc-mode)
		       (name . "^\\*rcirc.*")
		       (name . "^\\*ERC.*"))))))

  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-switch-to-saved-filter-groups "default")))
  :bind
  (:map ctl-x-map
	("C-b" . ibuffer-jump)))
(use-package eglot
  :defer t
  :after buffer-env
  :preface
  (defun my/eglot-eldoc ()
    (setq eldoc-documentation-strategy
	  'eldoc-documentation-compose-eagerly))
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
  (:map my/global-prefix-map
	("l a" . eglot-code-actions)
	("l b e" . eglot-events-buffer)
	("l b s" . eglot-stderr-buffer)
	("l f" . eglot-format)
	("l i" . eglot-inlay-hints-mode)
	("l l" . eglot)
	("l o" . eglot-code-action-organize-imports)
	("l r" . eglot-rename)
	("l s" . eglot-shutdown-all))
  (:repeat-map my/eglot-repeat-map
	       ("a" . eglot-code-actions)
	       ("b e" . eglot-events-buffer)
	       ("b s" . eglot-stderr-buffer)
	       ("f" . eglot-format)
	       ("i" . eglot-inlay-hints-mode)
	       ("l" . eglot)
	       ("o" . eglot-code-action-organize-imports)
	       ("r" . eglot-rename)
	       ("s" . eglot-shutdown-all))
  :config
  (if (eq system-type 'windows-nt)
      (setq exec-path (append exec-path '("~/.cache/emacs/lsp/pylsp/Scripts")))
    (setq exec-path (append exec-path '("~/.cache/emacs/lsp/pylsp/bin"))))
  :hook
  ((eglot-managed-mode . my/eglot-eldoc))
  )
(use-package buffer-env
  :ensure t
  :defer t
  :config
  (setq buffer-env-script-name ".venv/bin/activate"
	;; alternatively, try to find a .envrc file first
	buffer-env-script-name '(".envrc" ".venv/bin/activate"))
  :hook
  (prog-mode . buffer-env-update)
  )
(use-package python
  :defer t
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
  :bind
  (:map my/global-prefix-map
	("p c"	. python-shell-send-buffer)
	("p e"	. python-shell-send-statement)
	("p r"	. python-shell-send-region)
	("p p"	. run-python)
	("p z"	. python-shell-switch-to-shell)
	("p t c"	. python-skeleton-class)
	("p t d"	. python-skeleton-def)
	("p t f"	. python-skeleton-for)
	("p t i"	. python-skeleton-if)
	("p t t"	. python-skeleton-import)
	("p t w"	. python-skeleton-while))
  (:repeat-map my/python-repeat-map)

  :config
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  )
(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 1)
  (which-key-side-window-max-height 0.5)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  )
(use-package project
  :config
  (setq project-list-file (expand-file-name "cache/projects" user-emacs-directory) ; file to save knows projects
	)
  )
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
	    (lambda ()
	      (proced-toggle-auto-update 1))))
