(define-prefix-command 'my/global-prefix-map nil)
(keymap-set global-map "C-c" my/global-prefix-map)

(define-prefix-command 'my/emacs-prefix-map nil)
(keymap-set my/global-prefix-map "c" '("Core Emacs" . my/emacs-prefix-map))

(define-prefix-command 'my/lsp-prefix-map nil)
(keymap-set my/global-prefix-map "l" '("LSP" . my/lsp-prefix-map))

(define-prefix-command 'my/outline-prefix-map nil)
(keymap-set my/global-prefix-map "o" '("Outline" . my/outline-prefix-map))

(define-prefix-command 'my/shell-prefix-map nil)
(keymap-set my/global-prefix-map "s" '("Shell" . my/shell-prefix-map))


(use-package emacs
  :ensure nil
  :init
  (repeat-mode 1)
  (add-to-list 'exec-path (expand-file-name "bin" "~/.bun"))
  (add-to-list 'exec-path (expand-file-name "bin" "~/.local"))    
  :custom
  (repeat-exit-timeout 5) ; idle seconds after which turn of repeat mode
  :bind
  ("C-x C-k RET" . nil)			; disable kmacro edit
  ("C-x C-z" . nil)			; disable suspend frame
  ("C-z" . nil)				; disable suspend frame
  ("C-x x a" . append-to-buffer)
  ("C-x x p" . prepend-to-buffer)
  ("C-x x c" . copy-to-buffer)
  ("C-x x f" . append-to-file)
  (:repeat-map my/emacs-prefix-map
    	       ("j" . duplicate-dwim)
    	       (";" . comment-line)
    	       ("t" . transpose-lines)))

(use-package emacs			; echo area
  :ensure nil
  :config
  (setq message-log-max 100000))

(use-package emacs			; exiting emacs
  :ensure nil
  :config
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-message t))

(use-package emacs			; editing
  :ensure nil
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (size-indication-mode 1)
  :custom
  (line-move-visual nil)	 ; C-n C-p move by screen-lines
  (track-eol t)			 ; don't track end of line when moving
  (what-cursor-show-names t) ; show Unicode char name in what-cursor-position
  (undo-limit (* 16 1024 1024))		; in bytes
  (undo-strong-limit (* 16 1024 1024))
  (undo-outer-limit (* 16 1024 1024))
  :bind
  ("C-/" . undo-only)
  ("C-M-/" . undo-redo)
  )

(use-package minibuffer
  :ensure nil
  :defer t
  :custom
  (minibuffer-follows-selected-frame nil) ; minibuffer stays in same frame
  (insert-default-directory t) ; start with default directory in minibuffer
  (max-mini-window-height 0.25)	  ; default value, 25% of frame height
  (resize-mini-windows t)     ; resize mini-buffer based on text in it
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
  (completion-ignore-case t)		 ; case insensitive completion
  (read-buffer-completion-ignore-case t) ; ignore case for buffer name completion
  (read-file-name-completion-ignore-case t) ; ignore case for file name completion
  (minibuffer-default-prompt-format " [%s]") ; format string for default values
  (enable-recursive-minibuffer t)	     ; allow to minibuffer recursively
  (minibuffer-electric-default-mode t)	     ; remove default if user types

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
  (minibuffer-depth-indicate-mode t) ; show depth in case of recursion
  (setq
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

(use-package simple
  :ensure nil
  :custom
  (suggest-key-bindings 5)
  (extended-command-suggest-shorter t))

(use-package emacs			; help and info
  :ensure nil
  :custom
  (help-window-select t) ; switch to help window when created
  (help-window-keep-selected t) ; reuse same Help buffer
  )

(use-package emacs			; mark and region
  :ensure nil
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
  :ensure nil
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
  :ensure nil
  :defer t
  :custom
  (register-use-preview t)
  (register-preview-delay 1) ; seconds before displaying preview of register list
  )
(use-package bookmark
  :ensure nil
  :defer t
  :custom
  (bookmark-save-flag 1)     ; save bookmark to file automatically
  )

(use-package emacs			; display
  :ensure nil
  :custom
  (next-screen-context-lines 3) ; number lines that overlap during scroll command
  (scroll-conservatively 1000)	; never recenter point on redisplay
  (hscroll-margin 5)	; horizontally scroll long lines near the edge
  (hscroll-step 5)	; horizontally scroll only by small amount
  (hi-lock-auto-select-face t)	       ; don't prompt face
  (show-trailing-whitespace nil)       ; highlight trailing whitespace
  (line-number-display-limit nil) ; no size limit to display line numbers in mode line
  (display-line-numbers 'relative)	; relative line numbers
  (display-line-numbers-width nil) ; dynamically compute line number width
  (display-line-numbers-widen t)   ; show actual line number in narrow
  (display-raw-bytes-as-hex t)	   ; display raw bytes as hex
  (visible-bell t)		   ; don't beep but flash
  (truncate-lines t)	  ; truncate display of long lines, don't wrap
  (indicate-empty-lines t)		; show empty lines at the end of buffer
  :config
  (set-face-attribute 'default nil :height 120)
  (put 'scroll-left 'disabled nil)	; allow scrolling left
  (put 'narrow-to-region 'disabled nil) ; allow region narrowing
  (put 'narrow-to-page 'disabled nil)	; allow narrow to page
  (setq blink-cursor-blink -1)		; don't stop cursor blink
  (global-display-line-numbers-mode 1)
  )

(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq	search-ring-max 1000 ; search ring size
	search-exit-option t ; control chars end search
	isearch-allow-scroll 'unlimited ; allow screen scroll when in isearch
	regexp-search-ring-max 1000    ; regex search ring size
	search-default-mode t	       ; default regex search
	isearch-lazy-count t)	       ; show current match and total match number
  )
(use-package occur
  :ensure nil
  :defer t
  :hook
  (occur-mode . next-error-follow-minor-mode)	; auto enable follow mode
  (occur-mode . (lambda() (switch-to-buffer-other-window "*Occur*")))
  )

(use-package emacs			; letter case
  :ensure nil
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
  :ensure nil
  :defer t
  :config
  (setq ispell-personal-dictionary (expand-file-name "dictionary" user-emacs-directory) ; location of personal
	)
  :hook
  (text-mode-hook . flyspell-mode)	; fly-spell in text mode
  (prog-mode-hook . flyspell-prog-mode) ; fly-spell in progmode comment
  )

(use-package kmacro
  :ensure nil
  :defer t
  :config
  (setq	kmacro-ring-max 1000) ; macro ring size
  :config
  (if (file-exists-p (expand-file-name "macros" user-emacs-directory))
	  (load-file (expand-file-name "macros" user-emacs-directory)))
  )

(use-package emacs			; file handling
  :ensure nil
  :init
  (make-directory (expand-file-name "autosave/" user-emacs-directory) t)
  :custom
  (require-final-newline t)		; automatically add newline at the end of file
  (make-backup-files nil) ; don't create file backups
  (backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))) ; backup directory
  (auto-revert-verbose nil)	       ; don't flash echo area message
  (auto-revert-remote-files nil)	      ; disable for tramp
  (auto-revert-use-notify t)		      ; rely on file system notification
  (auto-revert-interval 5)		      ; poll for changes every 5 seconds
  (global-auto-revert-non-file-buffers t) ; auto revert for dired buffers etc
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t))) ; auto save directory
  (auto-save-list-file-prefix (expand-file-name "autosave/list" user-emacs-directory)) ; directory for recover session
  (delete-auto-save-files t) ; delete on buffer save
  (clean-buffer-list-delay-general 1) ; auto kill buffer after 1 day
  (delete-by-moving-to-trash t) ; delete from dired moves to trash
  (remote-file-name-inhibit-delete-by-moving-to-trash t) ; don't use trash for remote files
  (remote-file-name-inhibit-auto-save t)		 ; don't autosave remote files
  (remote-file-name-inhibit-locks t)			 ; don't create lock files
  (remote-file-name-inhibit-auto-save-visited t)	 ; don't create auto save files
  (remote-file-name-access-timeout 3)			 ; don't block emacs waiting for remote files
  (ange-ftp-generate-anonymous-password nil)		 ; prompt password
  (image-use-external-converter t)			 ; use image-magick for image not supported natively
  (image-converter 'imagemagick)			 ; use image-magick to convert
  :config
  (global-auto-revert-mode 1)	 ; auto update buffers if file changes
  )
(use-package recentf
  :ensure nil
  :defer t
  :custom
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 300)
  ;; (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-exclude (list #'file-remote-p))
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  :config
  (recentf-mode 1)
  :bind
  ("C-x f" . recentf-open)
  )

(use-package emacs			; buffers
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (clean-buffer-list-delay-general 1)	; number of days after which buffer is autokilled
  ;; :bind
  ;; ("C-x C-b" . buffer-menu-other-window)
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
  :ensure nil
  :defer t
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
	      ("C-j" . exit-minibuffer))
  :hook
  (after-init-hook . (lambda ()
		       (fido-mode -1)
		       (icomplete-vertical-mode 1)))
  :config
  (advice-add 'completion-at-point :after #'minibuffer-hide-completions) ; don't show "*completions*" buffer
  )

(use-package window
  :ensure nil
  :defer t
  :custom
  (switch-to-buffer-in-dedicated-window 'pop) ; in strongly dedicate windows behave like pop-to-buffer
  (switch-to-buffer-obey-display-actions t) ; C-x C-b respects display buffer rules
  :config
  (add-to-list
   'display-buffer-alist
   '((or . ((derived-mode . help-mode)
     	    (derived-mode . Info-mode)
     	    (derived-mode . apropos-mode)))
     (display-buffer-reuse-window display-buffer-in-side-window)
     (body-function . select-window)
     (inhibit-same-window . nil)
     (side . right)
     (window-width . 80)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   '((or . ((derived-mode . diff-mode)
     	    (derived-mode . ibuffer-mode)
     	    (derived-mode . proced-mode)))
     (display-buffer-reuse-window display-buffer-in-side-window)
     (body-function . select-window)
     (inhibit-same-window . nil)
     (side . right)
     (window-width . 0.5)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   '((or . ((derived-mode . shell-command-mode)
     	    (derived-mode . inferior-python-mode)
     	    (derived-mode . inferior-emacs-lisp-mode)
     	    (derived-mode . occur-mode)
     	    (derived-mode . grep-mode)
     	    (derived-mode . messages-buffer-mode)
     	    (derived-mode . xref--xref-buffer-mode)))
     (display-buffer-in-side-window)
     (body-function . select-window)
     (side . bottom)
     (window-height . 0.4)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   `(,(rx bos
	  (* anything)
   	  (or "shell"	      ; shell mode is set after display buffer
   	      "eshell"
   	      "term"))
     (display-buffer-reuse-window display-buffer-in-side-window)
     (body-function . select-window)
     (inhibit-same-window . nil)
     (side . bottom)
     (window-height . 0.5)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   `(,(rx bos
	  (* anything)
   	  (or "*vc-dir"			; vc-dir
   	      "*vc-log"			; commit message buffer
	        "*vc-git"		; git push
   	      "*Annotate"))		; vc-annotate
     (display-buffer-reuse-window display-buffer-in-side-window)
     (body-function . select-window)
     (side . bottom)
     (window-height . 0.4)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   `(,(rx "*log-edit")			; display files staged for commit
     (display-buffer-in-side-window)
     (body-function . select-window)
     (side . bottom)
     (window-height . 0.4)
     (window-width . 0.3)
     (slot . 1)))
  (add-to-list
   'display-buffer-alist
   `(,(rx bos
   	  "*eldoc")		 ; eldoc does not have a separate mode
     (display-buffer-in-side-window)
     (body-function . select-window)
     (side . bottom)
     (window-height . 0.4)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   '("\\*\\(Org Src\\).*"		; mode depends on source being edited
     (display-buffer-same-window)))
  (add-to-list
   'display-buffer-alist
   `(,(rx bos
	    "*Group")			; gnus email
     (display-buffer-in-tab)
     (ignore-current-tab . t)
     (tab-name . "Gnus")
     (tab-group . "Gnus")))
  (winner-mode 1)
  :bind
  ("M-o" . other-window)
  ("C-x w =" . balance-windows)
  ("C-x w u" . winner-undo)
  ("C-x w r" . winner-redo)
  ("C-x w 1" . delete-other-windows)
  ("C-x w {" . shrink-window-horizontally)
  ("C-x w }" . enlarge-window-horizontally)
  )

(use-package emacs			; frames
  :ensure nil
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
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  ;; distance between tab stops in columns. control width of tab
  ;; characters to display it should be positive integer and default
  ;; is 8
  (tab-width 8)
  (tab-first-completion 'eol)
  :config
  (electric-indent-mode 1)
  )

(use-package emacs			; text
  :ensure nil
  :custom
  (sentence-end-double-space nil)
  :hook
  (text-mode . turn-on-auto-fill)   ; automatic line breaking on space
  )
(use-package outline
  :ensure nil
  :defer t
  :custom
  (outline-blank-line t)
  (outline-minor-mode-use-buttons 'in-margins) ; show button in margin. pressing RET or click toggles fold
  (outline-minor-mode-cycle t)		; tab and s-tab on heading cycles fold
  (outline-default-state nil)		; don't fold to start with
  (outline-minor-mode-cycle-filter 'bolp) ; only cycle when point is at bol; elsewhere TAB acts as normal (indent-for-tab-command / completion-preview)
  :hook
  (prog-mode . outline-minor-mode)
  :bind
  (:repeat-map my/outline-prefix-map
               ("n"   . outline-next-visible-heading)
               ("p"   . outline-previous-visible-heading)
               ("f"   . outline-forward-same-level)
               ("b"   . outline-backward-same-level)
               ("u"   . outline-up-heading)

               ("a"   . outline-show-all) ; show All
               ("t"   . outline-hide-body) ; hide all body Text
               ("s"   . outline-show-subtree)
               ("d"   . outline-hide-subtree)
               ("e"   . outline-show-entry)
               ("c"   . outline-hide-entry)

               ("l"   . outline-hide-leaves) ; text of heading 
               ("k"   . outline-show-branches) ; heading itself, both does same in most cases

               ("q"   . outline-hide-sublevels)
               ("o"   . outline-toggle-children)

	       ("RET" . outline-insert-heading)
               ("/ h" . outline-hide-by-heading-regexp)
               ("/ s" . outline-show-by-heading-regexp)

               ("<"   . outline-promote)
               (">"   . outline-demote)
               ("^"   . outline-move-subtree-up)
               ("v"   . outline-move-subtree-down))
  )

(use-package imenu
  :ensure nil
  :defer t
  :custom
  (imenu-auto-rescan t)			; rescan buffer automatically
  (imenu-auto-rescan-maxout 100000000)
  (imenu-max-index-time 10)
  (imenu-sort-function 'imenu--sort-by-position)
  (imenu-flatten 'annotation)		; flatten list but show type as annotation
  (org-imenu-depth 10)
  )
(use-package emacs			; programs
  :ensure nil
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
  (electric-pair-skip-whitespace t)	       ; skip whitespace when skipping over closing paren
  (delete-pair-blink-delay 0.1)		       ; delete pair immediately
  (delete-pair-push-mark t)		       ; delete-pair pushes a mark at the end of delimited region
  :bind
  ("C-M-z" . delete-pair)
  :config
  (show-paren-mode 1)
  :hook
  (prog-mode . electric-pair-local-mode)
  (prog-mode . superword-mode) ; treat underscore as word char for navigation
  (c-mode . cwarn-mode)
  )
(use-package eldoc
  :ensure nil
  :defer t
  :custom
  (eldoc-help-at-pt t) ;; EMACS-31
  (eldoc-echo-area-display-truncation-message t) ; indicate if message was truncated
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly) ; show multiple documentation as soon as they are available
  (eldoc-idle-delay 0.5)	; wait before displaying documentation
  (eldoc-echo-area-use-multiline-p nil) ; don't allow multilne docs in echo area
  (eldoc-echo-area-prefer-doc-buffer t) ; reuse existing eldoc buffer
  :config
  (global-eldoc-mode 1)			; enable eldoc mode
  )
;; (use-package hideshow
;;   :ensure nil
;;   :defer t
;;   :custom
;;   (hs-isearch-open t) ; unhide code and comment if match is in hidden block during isearch
;;   (hs-hide-comments-when-hiding-all t) ; hide comments also when hs-hide-all
;;   :bind
;;   (:repeat-map my/hideshow-prefix-map
;;	       ("h" . hs-toggle-hiding)
;;	       ("c" . hs-hide-all)
;;	       ("o" . hs-show-all))
;;   :hook
;;   (prog-mode . hs-minor-mode)
;;   )
(use-package completion-preview
  :ensure nil
  :defer t
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
	("C-n" . completion-preview-next-candidate)
	("C-p" . completion-preview-prev-candidate)
	("TAB" . completion-preview-complete)
	("C-i" . completion-preview-insert))
  )

(use-package compile
  :ensure nil
  :defer t
  :custom
  (compilation-scroll-output 'first-error) ; scroll automatically
  (compilation-always-kill t) ; kill current compilation before starting new one
  (compilation-auto-jump-to-first-error nil) ; jump to first error
  (next-error-highlight 3)	 ; highlight error for 3 sec in source
  (next-error-highlight-no-select 3) ; highlight in non selected buffers in source
  (compilation-save-buffers-predicate 'ignore) ; don't save
  :config
  (add-hook 'compilation-finish-functions ; switch to compile buffer immediately
	    'switch-to-buffer-other-window 'compilation)
  :hook
  (comilation-mode . next-error-follow-minor-mode)
  )
(use-package grep
  :ensure nil
  :defer t
  :custom
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".venv" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  (grep-save-buffers 'ask)		 ; ask to save buffer
  (grep-use-null-filename-separator nil) ; don't use --null option of grep
  :hook
  (grep-mode . next-error-follow-minor-mode)
  )
(use-package flymake
  :ensure nil
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
  :ensure nil
  :defer t
  :custom
  (gud-tooltip-echo-area t)	       ; display tool tip in echo area
  (gdb-many-windows t)		       ; enable gdb many window mode
  )

(use-package vc
  :ensure nil
  :defer t
  :bind
  (:map vc-prefix-map
        ("c" . my/vc-git-clone)
        ("e" . vc-ediff))
  :custom
  (vc-revert-show-diff t)	      ; revert first shows diff buffer
  (vc-follow-symlinks t)	      ; follow symlinks
  (vc-command-messages t)	      ; log backend commands being run
  :hook
  (diff-mode . next-error-follow-minor-mode)	; auto enable follow mode
  :config
  (defun my/vc-git-clone (repository-url local-dir)
    "Run \"git clone REPOSITORY-URL\" to LOCAL-DIR.
    Executes `vc-dir' in the newly cloned directory."
    (interactive
     (let* ((url (read-string "Repository URL: "))
            (default-name (file-name-base url))
            (parent (read-directory-name "Clone into directory: " default-directory))
            (dir (expand-file-name default-name parent)))
       (list url dir)))
    (vc-git-command nil 0 nil "clone" repository-url
                    (directory-file-name local-dir))
    (vc-dir (expand-file-name local-dir)))    
  (add-to-list 'vc-directory-exclusion-list ".venv")
  )
(use-package project
  :ensure nil
  :defer t
  :custom
  (project-mode-line t)
  (project-list-file (expand-file-name "projects" user-emacs-directory))
  )
(use-package xref
  :ensure nil
  :defer t
  :custom
  (xref-auto-jump-to-first-definition 'show)
  (xref-search-program-alist '((grep . "xargs -0 grep <C> -snHE -e <R>"))) ; argument to xref-search-program
  )

(use-package abbrev
  :ensure nil
  :defer t
  :bind
  ("M-/" . 'hippie-expand)
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" "~/.config/emacs")) ; location to store personal abbrevs
  (save-abbrevs 'silently)		; save abbrev when file is saved
  (abbrev-suggest t)
  :config
  (if
	(file-exists-p abbrev-file-name)
	(quietly-read-abbrev-file))
  (abbrev-mode -1)			; don't expand automatically on space or punctuation
  )

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-alh")	       ; long human readable including dot files
  (dired-recursive-deletes 'top)    ; recursive delete confirm only for top level dir
  (dired-kill-when-opening-new-dired-buffer t) ; kill current buffer when opening new directoy
  (dired-dwim-target t)		       ; try to guess target directory
  (dired-create-destination-dirs 'ask) ; ask to create non existant directories when copying
  (dired-create-destination-dirs-on-trailing-dirsep t) ; trailing / treates destination as directory,
							; so rename directory actually moves into this directory
  (dired-copy-preserve-time t)		; preserve last modified time
  (dired-recursive-copies 'top)     ; recursive copy confirm only for top level dir
  (dired-vc-rename-file t)	    ; if under version control, use vc-rename-file
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  (ls-lisp-use-insert-directory-program nil) ; use ls-lisp instead of ls, useful for windows
  (dired-free-space nil)		     ; don't display free space
  ;; (dired-omit-files "\\`[.]\\|\\`[.]?#\\|\\`[.][.]?\\'") ; hide dot files in dired omit mode
  (dired-omit-verbose nil)		; don't spam echo area
  :hook
  (dired-mode . dired-omit-mode)	; hide . and ..
  )
(use-package wdired
  :ensure nil
  :defer t
  :commands (wdired-change-to-wdired-mode)
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  )
(use-package image-dired
  :ensure nil
  :defer t
  :custom
  (image-dired-thumbnail-storage 'standard)
  (image-dired-dir (expand-file-name "image-dired" user-emacs-directory))
  )

(use-package gnus
  :ensure nil
  :defer t
  :init
  (setq mail-user-agent 'gnus-user-agent
        read-mail-command #'gnus
        gnus-home-directory (expand-file-name "gnus/" user-emacs-directory)
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-interactive-exit t
        gnus-select-method '(nnnil nil)
        nnimap-record-commands t
        message-confirm-send t
        message-forward-as-mime t
        gnus-use-dribble-file t
        gnus-always-read-dribble-file t
        gnus-fetch-old-headers t
        gnus-large-newsgroup nil
        gnus-message-archive-group nil
        gnus-gcc-externalize-attachments nil
        gnus-gcc-mark-as-read t
        gnus-asynchronous t
        gnus-use-article-prefetch 5
        gnus-use-cache t
        gnus-use-header-prefetch t
        gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R"))
        gnus-summary-line-format "%U%R%3i %(%-18,18&user-date;  %-20,20f  %B%s%)\n"
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-leaf-with-other "├► "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-leaf "╰► "
        gnus-sum-thread-tree-vertical "│"
        gnus-summary-mode-line-format "[%U] %g"
        gnus-show-threads t
        gnus-thread-indent-level 2
        gnus-summary-make-false-root 'adopt
        gnus-summary-gather-subject-limit 'fuzzy
        gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
        gnus-thread-ignore-subject t
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-message-replysign t
        gnus-message-replyencrypt t
        gnus-message-replysign-encrypted t
        mm-verify-option 'known
        mm-decrypt-option 'known
        mm-sign-option nil
        mm-encrypt-option 'guided
        gnus-unbuttonized-mime-types nil)
  
  (defun my/gnus-close-tab ()
    (when (equal (alist-get 'name (tab-bar--current-tab)) "Gnus")
      (message "Closing tab %s" (alist-get 'name (tab-bar--current-tab)))
      (tab-bar-close-tab)))
  :bind
  ("C-c m" . gnus)
  :hook
  (message-mode . flyspell-mode)
  (gnus-exit-gnus . my/gnus-close-tab)
  :config
  (if (file-exists-p "~/.gnupg/authinfo.gpg")
      (load-file "~/etc/gnus_mail.el")))

(use-package message
  :ensure nil
  :defer t
  :after gnus
  :bind (:map message-mode-map
              ("C-c C-m a" . my/message-attach-pgp-key))
  :init
  (require 'epg)

  (defvar my/pgp-key-file (expand-file-name "~/.gnupg/share.asc")
    "Path to your armored public key, used for attaching to outgoing mail.
gpg --output public.pgp --armor --export username@email")

  (defun my/pgp-generate-key-file (key-id)
    "Generate an armored public key export for KEY-ID at `my/pgp-key-file' using EPG."
    (let* ((context (epg-make-context 'OpenPGP))
           (keys (epg-list-keys context key-id)))
      (unless keys
        (error "No key found matching: %s" key-id))
      (setf (epg-context-armor context) t)
      (with-temp-file my/pgp-key-file
        (insert (epg-export-keys-to-string context keys)))
      (message "Exported PGP key to %s" my/pgp-key-file)))

  (defun my/message-attach-pgp-key ()
    "Attach my PGP public key as a MIME part to the current message.
     If the key file doesn't exist yet, prompt for a key ID/email and generate it via EPG."
    (interactive)
    (unless (file-exists-p my/pgp-key-file)
      (my/pgp-generate-key-file
       (read-string "Key file not found. GPG key ID or email to export: ")))
    (mml-attach-file my/pgp-key-file "application/pgp-keys" "My PGP public key" "attachment")))

(use-package auth-source-xoauth2-plugin
  :ensure t
  :defer
  :custom
  (oauth2-token-file (expand-file-name "oauth2.plstore" "~/.gnupg"))
  :hook
  (gnus-before-startup . auth-source-xoauth2-plugin-mode)
  )

(use-package gnus-topic
  :ensure nil
  :after (gnus)
  :defer t
  :hook
  (gnus-group-mode . gnus-topic-mode)
  )

(use-package doc-view
  :ensure nil
  :defer t
  :custom
  (doc-view-resolution 200)
  (doc-view-continuous t)
  :config
  (add-hook 'doc-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  )

(use-package shell
  :ensure nil
  :defer t
  :custom
  (async-shell-command-display-buffer nil) ; display command buffer after command completion
  (async-shell-command-buffer 'new-buffer) ; create new buffer if there is already a buffer from another command
  (shell-command-prompt-show-cwd t)       ; show current dir in shell-command and async-shell-command
  :bind
  (:map my/shell-prefix-map
	("s" . shell)
	("e" . eshell)
	("t" . term))
  )

(use-package server
  :ensure nil
  :defer t
  :custom
  (server-stop-automatically nil)
  (server-use-tcp nil)
  (server-host nil)
  (server-port 9999)
  (server-kill-new-buffers t)
  )

(use-package saveplace
  :ensure nil
  :custom
  (save-place-limit 600)
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-forget-unreadable-files t)
  :config
  (save-place-mode 1)
  (defun my/save-place-ignore-remote (orig-fun &rest args)
    (unless (file-remote-p (or buffer-file-name default-directory))
      (apply orig-fun args)))
  (advice-add 'save-place-to-alist :around #'my/save-place-ignore-remote))

(ffap-bindings)

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-copy-size-limit (* 2 1024 1024)) ;; 2MB
  (tramp-use-scp-direct-remote-copying t)
  (enable-remote-dir-locals t)
  (tramp-verbose 2)
 	;; don't use auth-sources-search for completion. This conflicts with file name completion
  (tramp-completion-use-auth-sources nil)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path (expand-file-name "bin" "~/.bun"))
  (add-to-list 'tramp-remote-path (expand-file-name "bin" "~/.local"))
  (add-to-list 'tramp-remote-process-environment
		 (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
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
    (custom-theme-set-faces
     'modus-vivendi-tinted
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
     `(modus-themes-search-current ((,c :background "#ff5370" :foreground "#292D3E")))
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
  (load-theme 'modus-vivendi-tinted t)
  )

(use-package eglot
  :ensure nil
  :defer t
  :preface
  (defun my/eglot-eldoc ()
    (setq eldoc-documentation-strategy
  	  'eldoc-documentation-compose-eagerly))
  :custom
  (eglot-autoreconnect t "Automatically reconnect to LSP server")
  (eglot-connect-timeout 60 "Time out connection attempt after specified seconds")
  (eglot-sync-connect nil "Don't block Emacs user interface when connecting")
  (eglot-autoshutdown t "Shutdown language server when last buffer managed by it is killed")
  (eglot-confirm-server-initiated-edits nil "don't confirm server initiated edits with user")
  (eglot-ignored-server-capabilities nil "LSP capabilities that should not be used")
  (eglot-extend-to-xref t "activate eglot in non-project cross-referenced files")
  (eglot-send-changes-idle-time 1 "Send changes to LSP server after so many idle seconds")
  (eglot-report-progress nil "Don't spam echo area")
  :hook
  (prog-mode . eglot-ensure)
  ((eglot-managed-mode . my/eglot-eldoc))
  :config
  (defun my/org-babel-edit-prep (info) ; https://github.com/joaotavora/eglot/issues/523
    (let ((file (alist-get :file (caddr info))))
      (setq buffer-file-name
            (if file
		file
              (if (file-remote-p default-directory)
                  (concat (file-remote-p default-directory) "org-src-babel-tmp")
		"org-src-babel-tmp"))))
    (setq-local shell-file-name "/bin/sh")
    (eglot-ensure))

  (advice-add 'org-edit-src-code
  	      :before (defun my/org-edit-src-code/before (&rest args)
  			(when-let* ((element (org-element-at-point))
  				    (type (org-element-type element))
  				    (lang (org-element-property :language element))
  				    (mode (org-src-get-lang-mode lang))
  				    ((eglot--lookup-mode mode))
  				    (edit-pre (intern
  					       (format "org-babel-edit-prep:%s" lang))))
  			  (if (fboundp edit-pre)
  			      (advice-add edit-pre :after #'my/org-babel-edit-prep)
  			    (fset edit-pre #'my/org-babel-edit-prep)))))
  :bind
  (:repeat-map my/lsp-prefix-map
  	       ("a" . eglot-code-actions)
  	       ("b e" . eglot-events-buffer)
  	       ("b s" . eglot-stderr-buffer)
  	       ("c" . eglot-signal-didChangeConfiguration)
  	       ("f" . eglot-format)
  	       ("h" . eglot-inlay-hints-mode)
  	       ("l" . eglot)
  	       ("o" . eglot-code-action-organize-imports)
  	       ("r" . eglot-rename)
  	       ("R" . eglot-reconnect)
  	       ("s" . eglot-shutdown)
  	       ("S" . eglot-shutdown-all))
  )

(use-package treesit
  :ensure nil
  :defer t
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
	       ;; Note the version numbers. These are the versions that
	       ;; are known to work with Combobulate *and* Emacs.
	       '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
		 (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
		 (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
		 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
		 (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
		 (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
		 (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
		 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
		 (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
	(add-to-list 'treesit-language-source-alist grammar)
	;; Only install `grammar' if we don't already have it
	;; installed. However, if you want to *update* a grammar then
	;; this obviously prevents that from happening.
	(unless (treesit-language-available-p (car grammar))
	(treesit-install-language-grammar (car grammar)))))
  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
	     '((python-mode . python-ts-mode)
	       (css-mode . css-ts-mode)
	       (typescript-mode . typescript-ts-mode)
	       (js2-mode . js-ts-mode)
	       (bash-mode . bash-ts-mode)
	       (conf-toml-mode . toml-ts-mode)
	       (go-mode . go-ts-mode)
	       (css-mode . css-ts-mode)
	       (json-mode . json-ts-mode)
	       (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars))

(use-package python
  :ensure nil
  :defer t
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :init
  (if (string-equal system-type 'gnu/linux)
      (unless (file-exists-p (expand-file-name "uv" "~/.local/bin"))
        (async-shell-command "curl -LsSf https://astral.sh/uv/install.sh | sh")))
  (unless (file-exists-p (expand-file-name "pylsp" "~/.local/bin"))
    (async-shell-command "uv tool install python-lsp-server")
    (async-shell-command "uv tool install pyflakes"))

  (defun my/python-venv-setup ()
    "Set up a project-local .venv for exec-path, python-shell-interpreter,
      org-babel-python-command, and eglot/lsp-mode's pylsp jedi environment,
      buffer-locally.

      Works for both local and TRAMP-remote projects. `exec-path' gets the
      full (possibly remote-prefixed) path to .venv/bin, added without
      duplicates. `python-shell-interpreter', `org-babel-python-command',
      and the pylsp jedi environment get the *local* (non-TRAMP-prefixed)
      path, since those are consumed by processes that TRAMP itself spawns
      and runs remotely -- they must not contain the /ssh:host: prefix."
    (let* ((root (or (when (fboundp 'project-current)
                       (when-let ((proj (project-current)))
                         (if (fboundp 'project-root)
                             (project-root proj)
                           (car (project-roots proj)))))
                     default-directory))
           (venv-bin (expand-file-name ".venv/bin/" root)))
      (when (file-directory-p venv-bin)
      	;; --- exec-path: buffer-local, full (possibly remote) path, no dups ---
      	(make-local-variable 'exec-path)
      	(unless (member venv-bin exec-path)
          (push venv-bin exec-path))

      	;; --- interpreter / lsp paths: strip any TRAMP prefix ---
      	(let* ((local-venv-bin (file-local-name venv-bin))
               (interpreter (expand-file-name "python" local-venv-bin)))
          (when (file-executable-p (expand-file-name "python" venv-bin))
            (set (make-local-variable 'python-shell-interpreter) interpreter)
            (set (make-local-variable 'org-babel-python-command) interpreter)

	    ;; Eglot evaluates workspace config via this function globally so TRAMP/temp buffers work.
	    (setq-default eglot-workspace-configuration
			  (lambda (server)
			    (when-let* ((proj (project-current))
					(dir (project-root proj))
					(local-dir (file-local-name dir))
					(venv-py (expand-file-name ".venv/bin/python" local-dir))
					(full-venv-py (expand-file-name ".venv/bin/python" dir)))
			      (when (file-executable-p full-venv-py)
				`(:pylsp (:plugins (:jedi (:environment ,venv-py))))))))
    	    )))))


  :hook
  (python-base-mode . my/python-venv-setup)
  (org-mode . my/python-venv-setup)
  )

(use-package emacs			; custom file
  :ensure nil
  :defer t
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage)))

(use-package ibuffer
  :ensure nil
  :defer t
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
		       (name . "^\\.newsrc.*")
		       (name . "\\*imap log\\*")
		       (name . "^\\*BBDB\\*")))
	   ("eca" (name . "<eca.*>"))
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
  :ensure nil
  :defer nil
  :custom
  (which-key-idle-delay 1)
  (which-key-side-window-max-height 0.5)
  (which-key-max-description-length 0.3)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  )

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-hide-emphasis-markers t)			; hide bold, italic etc markers
  (org-goto-interface 'outline-path-completion) ; use completion
  (org-outline-path-complete-in-steps nil) ; flat navigation for org-goto
  (org-insert-heading-respect-content t) ; respect subtree when inserting next heading
  (org-export-use-babel nil)		; don't evaluate code during export
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
  :hook
  (org-mode . org-indent-mode)		; visually indent by outline structure
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((C . t)
				 (java . t)
				 (latex . t)
				 (lua . t)
				 (js . t)
				 (python . t)
				 (shell . t)
				 (emacs-lisp . t)))
  (setq org-confirm-babel-evaluate nil)	; don't ask when evaluating code blocks
  )

(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-filter 'user) ;; We can change interactively with `f'
  :config
  (add-hook 'proced-mode-hook
	    (lambda ()
	      (proced-toggle-auto-update 1))))

(use-package epg
  :ensure nil
  :defer t
  :custom
  (epg-pinentry-mode 'loopback)
  :config
  ;; Unset SSH_AGENT_PID by setting it to an empty string
  (setenv "SSH_AGENT_PID" "")
  ;; Set SSH_AUTH_SOCK to the output of the gpgconf command
  (setenv "SSH_AUTH_SOCK"
	  (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
  )

(use-package popper
  :ensure t ; or :straight t
  :defer t
  :init
  (setq popper-reference-buffers
	'("^\\*eshell.*\\*$"      eshell-mode
	  "^\\*shell.*\\*$"       shell-mode
	  "^\\*.*term.*\\*$"      term-mode
	  "^\\*Async Shell.*\\*$" shell-command-mode
	  inferior-python-mode
	  inferior-emacs-lisp-mode
	  compilation-mode
	  occur-mode
	  grep-mode
	  messages-buffer-mode
	  xref--xref-buffer-mode
	  "\\*\\(vc-dir\\|vc-log\\|Annotate\\).*"
	  "\\*\\(log-edit-\\).*"))
  :config
  (popper-mode +1)
  (setq popper-group-function #'popper-group-by-project) ; project.el projects
  (setq popper-display-control nil)	; honor display buffer alist
  (setq popper-echo-dispatch-keys nil) ; no short cut for specific popup window
  :bind (("<f12>"   . popper-toggle)
	 ("M-<f12>"   . popper-cycle)
	 ("C-<f12>" . popper-toggle-type))
  :hook
  (popper-mode . popper-echo-mode))	; For echo area hints

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-keep-variants t))

(use-package markdown-mode
  :ensure nil
  :defer t)

(use-package eww
  :ensure nil
  :defer t
  :init
  (defun my/browse-url-dispatch (url &rest args)
    "Open URL externally by default; with a prefix arg, open in eww."
    (if current-prefix-arg
	(eww-browse-url url)
      (apply #'browse-url-default-browser url args)))
  
  :custom
  (url-configuration-directory user-emacs-directory)
  (browse-url-browser-function #'my/browse-url-dispatch)
  :hook
  (eww-mode . visual-line-mode))
