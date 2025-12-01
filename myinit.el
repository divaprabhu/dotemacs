(define-prefix-command 'my/global-prefix-map nil "Global Prefix")
(keymap-set global-map "C-c" my/global-prefix-map)

(define-prefix-command 'my/core-prefix-map nil "Core Emacs Prefix")
(keymap-set my/global-prefix-map "c" '("Core Emacs Prefix" . my/core-prefix-map))

(define-prefix-command 'my/hideshow-prefix-map nil "Hideshow Prefix")
(keymap-set my/global-prefix-map "h" '("Hideshow Prefix" . my/hideshow-prefix-map))  

(define-prefix-command 'my/lsp-prefix-map nil "LSP Prefix")
(keymap-set my/global-prefix-map "l" '("LSP Prefix" . my/lsp-prefix-map))  

(define-prefix-command 'my/org-prefix-map nil "Org Prefix")
(keymap-set my/global-prefix-map "o" '("Org Prefix" . my/org-prefix-map))  

(define-prefix-command 'my/python-prefix-map nil "Python Prefix")
(keymap-set my/global-prefix-map "p" '("Python Prefix" . my/python-prefix-map))  

(define-prefix-command 'my/window-prefix-map nil "Window Prefix")
(keymap-set my/global-prefix-map "w" '("Window Prefix" . my/window-prefix-map))

(use-package emacs
  :init
  (repeat-mode 1)
  :custom
  (repeat-exit-timeout 5) ; idle seconds after which turn of repeat mode
  :bind
  ("C-x C-k RET" . nil)			; disable kmacro edit
  ("C-x z" . nil)			; disable suspend frame
  ("C-z" . nil)				; disable suspend frame
  ("C-x x a" . append-to-buffer)
  ("C-x x p" . prepend-to-buffer)
  ("C-x x c" . copy-to-buffer)
  ("C-x x f" . append-to-file)
  (:repeat-map my/core-prefix-map
	       ("j" . duplicate-dwim)
	       (";" . comment-line)
	       ("t" . transpose-lines)))

(use-package emacs			; echo area
  :config
  (setq message-log-max 100000))

(use-package emacs			; exiting emacs
  :bind
  ("C-x C-k RET" . nil)			; disable kmacro edit
  ("C-x z" . nil)			; disable suspend frame
  ("C-z" . nil)				; disable suspend frame
  :config
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-message t))

(use-package emacs			; editing
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (size-indication-mode 1)
  ;; (undo-limit (* 13 160000))
  ;; (undo-strong-limit (* 13 240000))
  ;; (undo-outer-limit (* 13 24000000))
  :custom
  (line-move-visual nil)	 ; C-n C-p move by screen-lines
  (track-eol t)			 ; don't track end of line when moving
  (what-cursor-show-names t) ; show Unicode char name in what-cursor-position
  )

(use-package minibuffer
  :custom
  (minibuffer-follows-selected-frame nil) ; minibuffer stays in same frame
  (insert-default-directory t) ; start with default directory in minibuffer
  (max-mini-window-height 0.25)	  ; default value, 25% of frame height
  (resize-mini-windows t)     ; resize mini-buffer based on text in it
  (minibuffer-depth-indicate-mode t) ; show depth in case of recursion
  (minibuffer-completion-auto-choose t) ; insert current completion candidate in mini-buffer
  (confirm-nonexistent-file-or-buffer nil) ; don't ask confirmation
  (use-short-answers t)		     ; use y or n instead of yes or no

  ;; mini-buffer completion
  (completion-styles '(partial-completion flex initials)) ; completion styles
  (completion-auto-help 'lazy) ; show completion buffer if can't complete
  (completion-auto-select 'second-tab) ; select completion buffer on second tab
  (completion-show-help nil)	       ; no help in completion buffer
  (completion-cycle-threshold 10) ; always cycle if number of completions is less than this number
  (completions-format 'one-column)     ; completion list buffer format
  (completions-sort 'historical) ; sort alphabetically and then by history
  (completions-max-height 10) ; height limit for completion list buffer
  (completions-header-format nil) ; no header in completion list buffer
  (completions-detailed t) ; display completions with details. Useful in describe-function etc
  (read-buffer-completion-ignore-case t) ; ignore case for buffer name completion
  (read-file-name-completion-ignore-case t) ; ignore case for file name completion
  (minibuffer-default-prompt-format " [%s]") ; format string for default values

  ;; mini-buffer history
  (history-length 1000)		; minibuffer history length
  (history-delete-duplicates t)	; remove duplicates
  (savehist-file (expand-file-name "savehist" user-emacs-directory)) ; location of minibuffer history file
  (savehist-additional-variables '(kill-ring      ; clipboard
				   register-alist ; macros
				   mark-ring global-mark-ring ; marks
				   search-ring regexp-search-ring)) ; searches

  (isearch-resume-in-command-history t) ; add isearch-resume command to command history
  :config
  (setq
   enable-recursive-minibuffer t ; allow to use mini-buffer recursively
   minibuffer-electric-default-mode t

   ;; mini-buffer completion
   completion-eager-update t	   ;
   completion-ignore-case t	   ; case insensitive completion

   ;; mini-buffer history
   savehist-minibuffer-history-variables '(minibuffer-history
					   query-replace-history
					   file-name-history
					   buffer-name-history
					   regexp-history
					   extended-command-history
					   shell-command-history
					   read-expression-history
					   command-history)
   )

  ;; mini-buffer
  (file-name-shadow-mode 1) ; shadow ignored file path in mini-buffer

  ;; history
  (savehist-mode 1)			; save minibuffer history
  :bind
  ("<escape>" . keyboard-escape-quit)
  )

(use-package emacs			; help and info
  :custom
  (help-window-select t) ; switch to help window when created
  (help-window-keep-selected t) ; reuse same Help buffer
  )

(use-package emacs			; mark and region
  :custom
  (highlight-nonselected-windows nil) ; each window highlights its own region
  (use-empty-active-region nil) ; region aware commands treat empty region as inactive
  (delete-active-region 'kill)	; kill region instead of delete
  (mark-even-if-inactive nil)	; don't use mark if region is inactive
  (set-mark-command-repeat-pop t) ; C-u C-SPC C-SPC... keeps popping local mark
  (mark-ring-max 512)		  ; local mark ring size
  (global-mark-ring-max 512)      ; global mark ring size
  :config
  (delete-selection-mode 1)    ; typing with region active replaces it


  )

(use-package emacs			; killing and moving text
  :custom
  (kill-do-not-save-duplicates t) ; don't save duplicates in kill ring
  (kill-whole-line t)	; kill-line at line start deletes newline also
  (kill-read-only-ok t) ; no error in read-only buffer, add to clipboard
  (kill-ring-max 1000)  ; kill ring size
  (save-interprogram-paste-before-kill t) ; save clipboard to kill ring before replacing it with kill
  :config
  (defun my/kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
  	(kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))
  (substitute-key-definition 'kill-region 'my/kill-region-or-backward-word (current-global-map))
  )

(use-package register
  :custom
  (register-use-preview t)
  (register-preview-delay 1) ; seconds before displaying preview of register list
  )
(use-package bookmark
  :custom
  (bookmark-save-flag 1)     ; save bookmark to file automatically
  )

(use-package emacs			; display
  :custom
  (next-screen-context-lines 3) ; number lines that overlap during scroll command
  (scroll-conservatively 1000)	; never recenter point on redisplay
  (hscroll-margin 5) ; horizontally scroll long lines near the edge
  (hscroll-step 5)	 ; horizontally scroll only by small amount
  (hi-lock-auto-select-face t)		; don't prompt face
  (show-trailing-whitespace nil)		; highlight trailing whitespace
  (line-number-display-limit nil) ; no size limit to display line numbers in mode line
  (display-line-numbers 'relative)	; relative line numbers
  (display-line-numbers-width nil) ; dynamically compute line number width
  (display-line-numbers-widen t)   ; show actual line number in narrow
  (display-raw-bytes-as-hex t)	; display raw bytes as hex 
  (visible-bell t)		 ; don't beep but flash
  (truncate-lines t)		 ; truncate display of long lines, don't wrap
  :config
  (put 'scroll-left 'disabled nil)	; allow scrolling left
  (put 'narrow-to-region 'disabled nil) ; allow region narrowing
  (put 'narrow-to-page 'disabled nil)	; allow narrow to page
  (setq-default indicate-empty-lines t) ; show blank lines at the end of buffer
  (setq blink-cursor-blink -1)		; don't stop cursor blink
  (global-display-line-numbers-mode 1)

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
(use-package occur
  :hook
  (occur-mode . next-error-follow-minor-mode)	; auto enable follow mode
  (occur-mode . (lambda() (switch-to-buffer-other-window "*Occur*")))
  )

(use-package emacs			; letter case
  :bind
  ;; change case commands map to dwim variant which apply to region if selected
  ("M-l" . 'downcase-dwim)
  ("M-u" . 'upcase-dwim)
  ("M-c" . 'capitalize-dwim)
  :config
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
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

(use-package emacs			; file handling
  :init
  (make-directory (expand-file-name "autosave/" user-emacs-directory) t)
  :custom
  (make-backup-files nil) ; don't create file backups
  (backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))) ; backup directory
  (auto-revert-verbose nil)	       ; don't flash echo area message
  (global-auto-revert-non-file-buffers t) ; auto revert for dired buffers etc
  (auto-revert-remote-files nil)	      ; disable for tramp
  (auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory)) ; auto-save directory
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))
  (delete-auto-save-files t) ; delete on buffer save
  (clean-buffer-list-delay-general 1) ; auto kill buffer after 1 day
  (delete-by-moving-to-trash t) ; delete from dired moves to trash
  (remote-file-name-inhibit-delete-by-moving-to-trash t) ; don't use trash for remote files
  (remote-file-name-inhibit-auto-save t)		 ; don't autosave remote files
  (remote-file-name-inhibit-locks t)			 ; don't create lock files
  (remote-file-name-inhibit-auto-save-visited t)	 ; don't create auto save files
  (image-use-external-converter t)			 ; use image-magick for image not supported natively
  :config
  (global-auto-revert-mode 1)	 ; auto update buffers if file changes
  )
(use-package recentf
  :custom
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  ;; (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  :config
  (recentf-mode 1)
  :bind
  ("C-x f" . recentf-open)
  )

(use-package emacs			; buffers
  :custom
  (clean-buffer-list-delay-general 1)	; number of days after which buffer is autokilled
  :config
  ;; A Protesilaos life savier HACK
  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff (if you choose `d') of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
	       (list "d"
		     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
		     "show diff between the buffer and its file"))
  )   
(use-package icomplete
  :custom
  (icomplete-delay-completions-threshold 0) ; pending completion number to apply icomplete-compute-delay
  (icomplete-compute-delay 0)
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil)
  (icomplete-prospects-height 10)
  (icomplete-separator " . ")
  (icomplete-with-completion-tables t)
  (icomplete-in-buffer t)		; in buffer completions
  (icomplete-max-delay-chars 0)
  (icomplete-scroll t)			; scroll instead of rotate
  :bind (:map icomplete-minibuffer-map
	      ("C-n" . icomplete-forward-completions)
	      ("C-p" . icomplete-backward-completions)
	      ("RET" . icomplete-force-complete-and-exit)
	      ;; to ignore icomplete and take what is entered literally
	      ;; we can exit commands like `multi-file-replace-regexp-as-diff'
	      ("C-j" . exit-minibuffer))
  :hook
  (after-init-hook . (lambda ()
		       (fido-mode -1)
		       (icomplete-vertical-mode 1)))
  :config
  (advice-add 'completion-at-point :after #'minibuffer-hide-completions) ; don't show "*completions*" buffer
  )

(use-package window
  :defer nil
  :custom
  (switch-to-buffer-in-dedicated-window 'pop) ; in strongly dedicate windows behave like pop-to-buffer
  (switch-to-buffer-obey-display-actions t) ; C-x C-b respects display buffer rules
  :config
  (setq display-buffer-alist
	'(("\\*\\(Metahelp\\|info\\|Help\\|Apropos\\).*"
	   (display-buffer-reuse-window display-buffer-in-side-window)
	   (side . right)
	   (window-width . 0.5)
	   (slot . 1))
	  ("\\*\\(.*shell\\|.*ansi-term\\|.*eshell\\|.*terminal\\|Async Shell\\).*"
	   (display-buffer-in-side-window)
	   (side . bottom)
	   (window-height . 0.5)
	   (slot . 0))
	  ("\\*\\(Messages\\|Output\\).*"
	   (display-buffer-in-side-window)
	   (side . bottom)
	   (window-width . 0.4)
	   (slot . 0))
	  ("\\*\\(vc-dir\\|vc-log\\|Annotate\\).*"
	   (display-buffer-reuse-window display-buffer-in-side-window)
	   (side . bottom)
	   (window-height . 0.6)
	   (slot . 0))
	  ("\\*\\(log-edit-\\|vc-git\\).*"
	   (display-buffer-in-atom-window)
	   (side . right)
	   (window-width . 0.3)
	   (slot . 0))
	  ("\\*\\(Diff\\|vc-diff\\).*"
	   (display-buffer-in-side-window)
	   (side . bottom)
	   (window-height . 0.6)
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
	   (slot . 1))
	  ("\\*\\(Proced\\).*"
	   (display-buffer-in-side-window)
	   (side . bottom)
	   (window-height . 0.5)
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
	   (slot . 2))
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
 
  (winner-mode)
  :bind
  ("M-o" . other-window)
  (:repeat-map my/window-prefix-map
	       ("0" . delete-window)
	       ("1" . delete-other-windows)
	       ("=" . balance-windows)
	       ("o" . other-window)
	       ("r" . winner-redo)
	       ("t" . window-toggle-side-windows)
	       ("u" . winner-undo))
  ("{" . shrink-window-horizontally)
  ("}" . enlarge-window-horizontally)
  )

(use-package emacs			; frames
  :custom
  (tty-menu-open-use-tmm -1)
  (tab-bar-show t)
  (use-dialog-box nil)  ; don't show dialog box, use mini-buffer
  (tooltip-mode -1)
  (fringe-mode -1)
  (frame-resize-pixelwise t)
  (frame-inhibit-implied-resize t)
  :config
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tab-bar-mode -1)
  (setq frame-title-format
	'(:eval
	  (let ((project (project-current)))
            (if project
		(concat " "
			(file-name-nondirectory (directory-file-name (project-root project))))
              (concat " " (buffer-name))))))
  )

(use-package emacs			; indentation
    :custom
    (tab-always-indent 'complete)
    ;; distance between tab stops in columns. control width of tab characters to display
    ;; it should be positive integer and default is 8
    (tab-width 8)
)

(use-package emacs			; text
    :hook
      (text-mode . turn-on-auto-fill)   ; automatic line breaking on space
)

(use-package imenu
  :custom
  (imenu-auto-rescan t)			; rescan buffer automatically
  )
(use-package emacs			; programs
  :custom
  (blink-matching-paren 'jump)	 ; briefly move to matching open paren
  (blink-matching-delay 1)	 ; not used in show paren mode
  (show-paren-highlight-openparen t) ; highlight open paren when point is just before it
  (show-paren-delay 0)	   ; time in sec before showing matching paren
  (show-paren-style 'parenthesis)	; highlight matching paren
  (show-paren-when-point-inside-paren t) ; highlight when point is inside paren
  (show-paren-context-when-offscreen t)	; show some context in echo when open paren is offscreen
  (electric-pair-preserve-balance t)	; balance paren
  (electric-pair-delete-adjacent-pairs t) ; backspace of open paren also deletes close paren when both are nearby
  (electric-pair-open-newline-between-pairs t) ; newline between adjacent parens open new one
  :config
  (show-paren-mode 1)
  :hook
  (prog-mode . electric-pair-local-mode)
  (prog-mode . superword-mode) ; treat underscore as word char for navigation
  (c-mode . cwarn-mode)
  )
(use-package eldoc
  :defer t
  :custom
  (eldoc-help-at-pt t) ;; EMACS-31
  (eldoc-echo-area-display-truncation-message t) ; indicate if message was truncated
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly) ; show multiple documentation as soon as they are available
  (eldoc-idle-delay 0.5)	; wait before displaying documentation
  (eldoc-echo-area-use-multiline-p t) ; don't allow multilne docs in echo area
  (eldoc-echo-area-prefer-doc-buffer t) ; reuse existing eldoc buffer
  :config
  (global-eldoc-mode 1)			; enable eldoc mode
  )
(use-package hideshow
  :custom
  (hs-isearch-open t) ; unhide code and comment if match is in hidden block during isearch
  (hs-hide-comments-when-hiding-all t) ; hide comments also when hs-hide-all
  :bind
  (:repeat-map my/hideshow-prefix-map
	       ("h" . hs-toggle-hiding)
	       ("c" . hs-hide-all)
	       ("o" . hs-show-all))
  :hook
  (prog-mode . hs-minor-mode)
  )
(use-package completion-preview
  :hook
  (prog-mode . 'completion-preview)
  (text-mode . 'completion-preview)
  (comint-mode . 'completion-preview)
  :custom
  (completion-preview-minimum-symbol-length 1) ; minimum number of chars to start completion
  :config
  (push 'org-self-insert-command completion-preview-commands)
  :bind
  (:map completion-preview-active-mode-map
	("M-n" . completion-preview-next-candidate)
	("M-p" . completion-preview-prev-candidate)
	("TAB" . completion-preview-complete)
	("M-i" . completion-preview-insert))
  )

(use-package compile
  :defer t
  :custom
  (compilation-scroll-output 'first-error) ; scroll automatically
  (compilation-always-kill t) ; kill current compilation before starting new one
  (compilation-auto-jump-to-first-error t) ; jump to first error
  (next-error-highlight 3)	 ; highlight error for 3 sec in source
  (next-error-highlight-no-select 3) ; highlight in non selected buffers in source
  (compilation-save-buffers-predicate 'ignore) ; don't save
  :config
  (add-hook 'compilation-finish-functions ; switch to compile buffer immediately
    	    'switch-to-buffer-other-window 'compilation)
  )
(use-package grep
  :defer t
  :custom
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".venv" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  (grep-save-buffers 'ask)		 ; ask to save buffer
  (grep-use-null-filename-separator nil) ; don't use --null option of grep
  )
(use-package flymake
  :defer t
  :bind (:map flymake-mode-map
  	      ("M-n" . 'flymake-goto-next-error)
  	      ("M-p" . 'flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout 3)	; wait 3 sec before checking
  (flymake-show-diagnostics-at-end-of-line nil) ; add diagnostic summary at end of line
  (flymake-start-on-flymake-mode t)  ; start checking when enabled
  (flymake-wrap-around t)	       ; wrap around
  (help-at-pt-display-when-idle t)   ; show local help on point over
  (help-at-pt-timer-delay 1)	       ; show help after 1 sec
  :config
  (remove-hook 'lisp-interaction-mode-hook 'flymake-mode)
  :hook
  (prog-mode . flymake-mode)
  )
(use-package gud
  :defer t
  :custom
  (gud-tooltip-echo-area t)	       ; display tool tip in echo area
  (gdb-many-windows t)		       ; enable gdb many window mode
  )

(use-package vc
  :defer t
  :custom
  (vc-revert-show-diff t)	      ; revert first shows diff buffer
  (vc-follow-symlinks t)	      ; follow symlinks
  (vc-command-messages t)	      ; log backend commands being run
  :hook
  (diff-mode . next-error-follow-minor-mode)	; auto enable follow mode
  )
(use-package project
  :custom
  (project-list-file (expand-file-name "projects" user-emacs-directory)) ; file to save knows projects
  )
(use-package xref
  :defer t
  :custom
  (xref-search-program-alist '((grep . "xargs -0 grep <C> -snHE -e <R>"))) ; argument to xref-search-program
  )

(use-package abbrev
  :defer t
  :bind
  ("M-/" . 'hippie-expand)
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)) ; location to store personal abbrevs
  (save-abbrevs 'silently)		; save abbrev when file is saved
  (abbrev-suggest t)
  :config
  (if
      (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (abbrev-mode -1)			; don't expand automatically on space or punctuation
  )

(use-package shell
  :defer t
  :custom
  (async-shell-command-display-buffer nil) ; display command buffer after command completion
  (async-shell-command-buffer 'new-buffer) ; create new buffer if there is already a buffer from another command
  (shell-command-prompt-show-cwd t)       ; show current dir in shell-command and async-shell-command
  )

(use-package desktop
  :demand t
  :init
  (setq desktop-dirname (expand-file-name user-emacs-directory))
  :custom
  (setq desktop-restore-eager 2) ; number of buffers to restore eagerly
  (desktop-lazy-idle-delay 2) ; idle delay for creating other buffers lazily
  (desktop-load-locked-desktop 'ask) ; notify if another emacs instance is locking session
  (desktop-restore-frames 1) ; save and restore frames and window config
  (desktop-save t)	     ; always save desktop when quitting emacs
  (desktop-path (list user-emacs-directory)) ; list of directories to search for desktop file
  (desktop-auto-save-timeout 60) ; idle time seconds before autosaving
  (desktop-base-file-name "emacs.desktop") ; desktop file name
  (desktop-globals-to-save		; global variables to be saved
   '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history))
  (desktop-locals-to-save		; local variables to be saved
   '(buffer-undo-list eww-history-position desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system buffer-display-time indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace))
  :config
  (desktop-save-mode t)			; save desktop
  )
(use-package saveplace
  :demand t
  :custom
  (save-place-limit 600)
  (save-place-file (expand-file-name "saveplace" user-emacs-directory)) ; file where place is stored
  (save-place-forget-unreadable-files t) ; set to nil if emacs is slow to exit
  :config
  (save-place-mode 1)			; enable saveplace mode
  )

(use-package tramp
  :custom
  (tramp-copy-size-limit (* 2 1024 1024)) ;; 2MB
  (tramp-use-scp-direct-remote-copying t)
  (enable-remote-dir-locals t)
  (tramp-verbose 2)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

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
     (bg-mode-line-active "#181818") ;"#232635"
     (bg-mode-line-inactive "#424242") ;"#282c3d"
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

(use-package eglot
  :defer t
  :preface
  (defun my/eglot-eldoc ()
    (setq eldoc-documentation-strategy
	  'eldoc-documentation-compose-eagerly))
  :custom
  (eglot-autoreconnect t "Automatically reconnect to LSP server")
  (eglot-connect-timeout 60 "Time out connection attempt after specified seconds")
  (eglot-sync-connect nil "Don't block Emacs user interface when connecting")
  (eglot-events-buffer-size 200000000 "Max number of chars on event buffer")
  (eglot-autoshutdown t "Shutdown language server when last buffer managed by it is killed")
  (eglot-confirm-server-initiated-edits nil "don't confirm server initiated edits with user")
  (eglot-ignored-server-capabilities nil "LSP capabilities that should not be used")
  (eglot-extend-to-xref t "activate eglot in non-project cross-referenced files")
  (eglot-send-changes-idle-time 1 "Send changes to LSP server after so many idle seconds")
  (eglot-report-progress nil "Don't spam echo area")
  :hook
  ((eglot-managed-mode . my/eglot-eldoc))
  :bind
    (:repeat-map my/lsp-prefix-map
	       ("a" . eglot-code-actions)
	       ("b e" . eglot-events-buffer)
	       ("b s" . eglot-stderr-buffer)
	       ("f" . eglot-format)
	       ("i" . eglot-inlay-hints-mode)
	       ("l" . eglot)
	       ("o" . eglot-code-action-organize-imports)
	       ("r" . eglot-rename)
	       ("s" . eglot-shutdown-all))
  )

(use-package python
  :defer t
  :init
  (let ((pylspdir (expand-file-name "lsp/pylsp" "~/.cache")))
    (unless (file-directory-p pylspdir)
      (make-directory pylspdir t)
      (cond
       ((eq system-type 'windows-nt)
	(async-shell-command (concat "python -m venv " pylspdir))
	(async-shell-command (concat pylspdir "/Scripts/activate.bat && pip install -U pip python-lsp-server[all]")))
       (t
	(async-shell-command (concat "python3 -m venv " pylspdir))
	(async-shell-command (concat ". " pylspdir "/bin/activate && pip install -U pip python-lsp-server[all]"))))))
  :config
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  :bind
    (:map my/python-prefix-map
	("c"	. python-shell-send-buffer)
	("e"	. python-shell-send-statement)
	("r"	. python-shell-send-region)
	("p"	. run-python)
	("z"	. python-shell-switch-to-shell)
	("t c"	. python-skeleton-class)
	("t d"	. python-skeleton-def)
	("t f"	. python-skeleton-for)
	("t i"	. python-skeleton-if)
	("t t"	. python-skeleton-import)
	("t w"	. python-skeleton-while))
  )

(use-package emacs			; custom file
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage)))

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
(:map ibuffer-mode-map
	   ("* f" . ibuffer-mark-by-file-name-regexp)
	   ("* g" . ibuffer-mark-by-content-regexp)
	   ("* n" . ibuffer-mark-by-name-regexp)
	   ("s n" . ibuffer-do-sort-by-alphabetic)
	   ("/ g" . ibuffer-filter-by-content)
	   ("M-o" . other-window))
   (:map ctl-x-map
	   ("C-b" . ibuffer-jump)))

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

(use-package org
  :defer t
  :custom
  (org-hide-emphasis-markers t)
  :bind
  (:repeat-map my/org-prefix-map
	       ("C-n" . outline-next-visible-heading)
	       ("C-p" . outline-previous-visible-heading)
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

(use-package dired
  :defer t
  :custom
  (dired-dwim-target t)		       ; try to guess target directory
  (dired-create-destination-dirs 'ask) ; ask to create non existant directories when copying
  (dired-kill-when-opening-new-dired-buffer t) ; kill current buffer when opening new directoy
  (dired-listing-switches "-alh")	       ; long human readable including dot files
  (dired-copy-preserve-time t)		; preserve last modified time
  (dired-recursive-copies 'top)     ; recursive copy confirm only for top level dir
  (dired-vc-rename-file t)	    ; if under version control, use vc-rename-file
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  (ls-lisp-use-insert-directory-program nil) ; use ls-lisp instead of ls, useful for windows
  (image-dired-dir (expand-file-name "cache/image-dired" user-emacs-directory))
  )
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t)
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
(proced-filter 'user) ;; We can change interactively with `f'
:config
(add-hook 'proced-mode-hook
	    (lambda ()
	      (proced-toggle-auto-update 1))))

(use-package doc-view
  :custom
  (doc-view-resolution 200)
  )

(use-package epg
  :defer t
  :custom
  (epg-pinentry-mode 'loopback)
  (transient-history-file )
  (auth-sources (expand-file-name "authinfo" user-emacs-directory))
  )

(use-package popper
  :ensure t ; or :straight t
  :config
  (setq popper-group-function #'popper-group-by-project) ; project.el projects
  (setq popper-display-control nil)	; honor display buffer alist
  (setq popper-echo-dispatch-keys nil) ; no short cut for specific popup window
  :bind (("<f12>"   . popper-toggle)
	 ("M-<f12>"   . popper-cycle)
	 ("C-<f12>" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
	'("\\*\\(Metahelp\\|info\\|Help\\|Apropos\\).*"
	  "\\*\\(.*shell\\|.*ansi-term\\|.*eshell\\|.*terminal\\|Async Shell\\).*"
	  "\\*\\(Messages\\|Output\\).*"
	  "\\*\\(vc-dir\\|vc-log\\|Annotate\\).*"
	  "\\*\\(log-edit-\\).*"
	  "\\*\\(Diff\\|vc-diff\\).*"
	  "\\*\\(Open Recent\\).*"
	  "\\*\\(Ibuffer\\).*"
	  "\\*\\(Proced\\).*"
	  "\\*\\(Embark\\).*"
	  "\\*\\(eldoc\\|xref\\|Flymake\\).*"
	  "\\*\\(Python\\|ielm\\).*"
	  "\\*\\(compilation\\|Occur\\|grep\\).*"
          "Output\\*$"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
