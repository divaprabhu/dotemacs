(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  (expand-file-name (convert-standard-filename "eln-cache/")
			    user-emacs-directory)))

(setq inhibit-startup-screen t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(package-initialize)
(if (< emacs-major-version 28)
    (add-to-list 'package-archives
		 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

(setq next-line-add-newline t)

(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(size-indication-mode 1)
(setq what-cursor-show-names t)

(setq resize-mini-windows t)

(setq enable-recursive-minibuffer t)
(setq minibuffer-depth-indicate-mode t)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq completion-cycle-threshold t)

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

(setq suggest-key-bindings 5)
(setq extended-command-suggest-shorter t)

(setq highlight-nonselected-windows t)

(setq mark-even-inactive nil)

(setq set-mark-command-repeat-pop t)

(unless (package-installed-p 'expand-region)
  (package-refresh-contents)
  (package-install 'expand-region))

(global-set-key (kbd "C-+") 'er/expand-region)
(global-set-key (kbd "C-_") 'er/contract-region)

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
(put 'scroll-left 'disabled nil)

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

(setq blink-cursor-blink -1)

(setq-default display-line-numbers 'relative)
(setq-default display-line-numbers-width nil)
(setq display-raw-bytes-as-hex t)
(setq visible-bell t)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(setq isearch-lazy-count t)

(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'flyspell-mode-hook
	  '(lambda()
	     (define-key flyspell-mode-map (kbd "C-M-i") nil)))
(setq ispell-personal-dictionary (expand-file-name "dictionary" user-emacs-directory))

(if (file-exists-p (expand-file-name "macros" user-emacs-directory))
    (load-file (expand-file-name "macros" user-emacs-directory)))

(setq make-backup-files nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(setq global-auto-revert-non-file-buffers t
      auto-revert-remote-files t)
(global-auto-revert-mode 1)

(make-directory (expand-file-name "autosave/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "autosave/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))

(setq delete-by-moving-to-trash t)

(setq completion-styles '(initials partial-completion flex basic))
(if (>= emacs-major-version 29)
    (progn
      (icomplete-vertical-mode 1)
      (fido-vertical-mode 1)))

(setq help-window-select t
      switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)
(add-hook 'occur-hook
	  '(lambda ()
	     (switch-to-buffer-other-window "*Occur*")))
;;(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)
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
	("\\*\\(Messages|Output\\).*"
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

(setq tab-always-indent 'complete)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq imenu-auto-rescan t)

(which-function-mode 1)

(setq blink-matching-paren 'jump
      blink-matching-delay 1)		; not used in show paren mode

(setq show-paren-highlight-openparen t
      show-paren-style 'mixed
      show-paren-when-point-inside-paren t)
(show-paren-mode 1)

(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs t)

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

(setq grep-save-buffers nil
      grep-use-null-filename-separator nil)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(setq flymake-no-changes-timeout nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 1)

(setq gud-tooltip-echo-area t)

(setq gdb-many-windows t)

(setq initial-scratch-message nil)

(defun efs/ielm-send-line-or-region ()
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

(defun efs/show-ielm ()
  (interactive)
  (select-window (split-window-vertically -10))
  (ielm)
  (text-scale-set 1))

(define-key org-mode-map (kbd "C-M-x") 'efs/ielm-send-line-or-region)

(setq vc-follow-symlinks t)
(setq vc-command-messages t)

(setq read-file-name-completion-ignore-case t
      xref-search-program-alist '((grep . "xargs -0 grep <C> -snHE -e <R>")
				  ))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq split-window-function 'split-window-horizontally)
(setq ediff-keep-variants nil)

(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)
(if
    (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(abbrev-mode -1)			; don't expand automatically on space or punctuation

(global-set-key (kbd "M-/") 'hippie-expand)

(require 'dired)
(require 'ls-lisp)
(setq dired-create-destination-dirs 'ask
      dired-dwim-target t
      dired-create-destination-dirs t
      dired-copy-preserve-time t
      dired-recursive-copies t
      dired-vc-rename-file t
      ls-lisp-use-insert-directory-program nil) ; use ls-lisp instead of ls
(define-key dired-mode-map (kbd "M-+") 'dired-create-empty-file)

(setq async-shell-command-display-buffer nil
      shell-command-prompt-show-cwd t)

(setq desktop-restore-eager 2
      desktop-load-locked-desktop 'ask
      desktop-restore-frames 1
      desktop-save t
      desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 60
      desktop-base-file-name "emacs.desktop"
      desktop-globals-to-save
      '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
      desktop-locals-to-save
      '(eww-history-position desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system buffer-display-time indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace))
(desktop-save-mode t)

(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1)

(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-forget-unreadable-files t) ; set to nil if emacs is slow to exit

(ffap-bindings)

;; don't create newsrc file that other clients may use
(setq gnus-save-newsrc-file t)
(setq gnus-read-newsrc-file t)

;; save emacs specific newsrc file in cache
(setq gnus-startup-file (expand-file-name "newsrc" user-emacs-directory))

;;save dribble file in cache and read on start up without prompt if exists
(setq gnus-dribble-directory user-emacs-directory)
(setq gnus-always-read-dribble-file t)

;; about you
(setq user-mail-address "name@domain.com"
      user-full-name "Divakar V Prabhu")

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

;; define everything via secondary method
;; read credentials from authinfo.gpg
(setq gnus-select-method '(nnnil nil))
(setq gnus-secondary-select-methods
      '((nntp   "gwene"
		(nntp-address "news.gwene.org"))))
(if (file-exists-p "~/etc/gnupg/authinfo.gpg")
    (load-file "~/etc/gnus_mail.el"))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

(setq gnus-fetch-old-headers t)

(setq-default
 gnus-summary-line-format "%U%R%z%I%(%&user-date; [%4L: %-23,23f%]%) %s\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

(add-hook 'gnus-summary-mode-hook 'my-summary-mode-map)
(add-hook 'gnus-article-prepare-hook 'my-summary-mode-map)
(defun my-summary-mode-map ()
  (local-set-key "e" "MMen")
  (local-set-key "E" 'gnus-summary-edit-article))

(setq nnmail-expiry-wait 'immediate)

(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 0.25
			 (group 1.0))
	       (vertical 1.0
			 (summary 0.25 point)
			 (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 0.25
			 (group 1.0))
	       (vertical 1.0
			 (summary 1.0 point)))))

(setq epg-pinentry-mode 'loopback)

(setq url-configuration-directory user-emacs-directory)
(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-mode-hook 'visual-line-mode)

(require 'org-tempo)

(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (python . t)))
(setq org-confirm-babel-evaluate nil)

(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))
(require 'modus-themes)
(setq modus-themes-inhibit-reload nil   ; reload active theme when an option is changed through the Customize UI
      modus-themes-bold-constructs t    ; Use bold for code syntax highlighting and related
      modus-themes-italic-constructs t  ; Use bold for code syntax highlighting and related
      modus-themes-mode-line '(accented borderless (padding 4) (height 0.9))
      modus-themes-hl-line '(intense)   ; amplify color in use for hl-line-mode
					; heading sizes and colors
      modus-themes-headings '((1 . (bold rainbow 1.5))
			      (2 . (bold rainbow 1.4))
			      (3 . (bold rainbow 1.2))
			      (t . (monochrome ))))
(load-theme 'modus-vivendi t)

(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

;; (unless (package-installed-p 'goto-chg)
;;   (package-refresh-contents)
;;   (package-install 'goto-chg))

(unless (package-installed-p 'evil-collection)
  (package-refresh-contents)
  (package-install 'evil-collection))

(setq evil-want-C-i-jump t                 ; use C-i for jump list navigation as complement to C-o
      evil-want-C-u-scroll t               ; C-u scroll up in normal mode
      evil-move-beyond-eol t               ; move one char beyond end of line
      evil-cross-lines t                   ; motions like h, l, f can go to next/prev line
      evil-respect-visual-line-mode t      ; respect visual-line-mode so that j, k move by visual lines
      evil-show-paren-range 1              ; distance from parenthesis to highlight it
      evil-want-fine-undo t                ; use Emacs heuristics for undo
      evil-disable-insert-state-bindings t ; use emacs bindings in insert mode
      evil-want-keybinding nil             ; required for evil-collection
      evil-want-integration t              ; required for evil-collection
      )
					; load evil
(require 'evil)
(evil-set-undo-system 'undo-redo)          ; native to Emacs 28

(add-hook 'prog-mode-hook 'turn-on-evil-mode)
(add-hook 'text-mode-hook 'turn-on-evil-mode)
(evil-collection-init '(corfu dired ediff eglot eldoc flymake go-mode neotree org python restclient unimpaired vc-annotate vc-dir vc-git xref youtube-dl))
;; (evil-mode 1)
;; (evil-collection-init)

(require 'ibuffer)
(setq ibuffer-expert t
      ibuffer-display-summary nil
      ibuffer-use-other-window nil
      ibuffer-show-empty-filter-groups nil
      ibuffer-movement-cycle nil
      ibuffer-default-sorting-mode 'filename/process
      ibuffer-use-header-line t
      ibuffer-default-shrink-to-minimum-size nil
      ibuffer-formats
      '((mark modified read-only locked " "
	      (name 40 40 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " " filename-and-process)
	(mark " "
	      (name 16 -1)
	      " " filename))
      ibuffer-saved-filter-groups nil
      ibuffer-old-time 48)
(define-key ibuffer-mode-map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
(define-key ibuffer-mode-map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
(define-key ibuffer-mode-map (kbd "* n") #'ibuffer-mark-by-name-regexp)
(define-key ibuffer-mode-map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
(define-key ibuffer-mode-map (kbd "/ g") #'ibuffer-filter-by-content)
(define-key ctl-x-map (kbd "C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      '(("default"
	 ("dired" (mode . dired-mode))
	 ("perl" (mode . cperl-mode))
	 ("c" (mode . c-mode))
	 ("python" (mode . python-mode))
	 ("org" (mode . org-mode))
	 ("vc" (or
		(name . "vc-.*")
		(name . "^\\*log-edit-files\\*$")))
	 ("emacs" (or
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Messages\\*$")))
	 ("svg" (name . "\\.svg")) ; group by file extension
	 ("gnus" (or
		  (mode . message-mode)
		  (mode . bbdb-mode)
		  (mode . mail-mode)
		  (mode . gnus-group-mode)
		  (mode . gnus-summary-mode)
		  (mode . gnus-article-mode)
		  (name . "^\\.bbdb$")
		  (name . "^\\.newsrc-dribble"))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

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
;;(define-key vertico-map "?" #'minibuffer-completion-help)
;;(define-key vertico-map (kbd "RET") #'minibuffer-force-complete-and-exit)
;;(define-key vertico-map (kbd "TAB") #'minibuffer-complete)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))
(which-key-mode 1)

(unless (package-installed-p 'embark)
  (package-refresh-contents)
  (package-install 'embark))
(require 'embark)
(setq prefix-help-command #'embark-prefix-help-command)

(unless (package-installed-p 'corfu)
  (package-refresh-contents)
  (package-install 'corfu))
(require 'corfu)

(global-corfu-mode 1)
(setq corfu-preselct-first nil	; disable candidate preselection
      corfu-auto t			; enable auto completion
      corfu-auto-delay 1		; delay for auto completion
      corfu-quit-no-match 'separator	; stay alive when starting advanced match with corfu separator even if no match
      corfu-quit-at-boundary nil	; don't quit at completion boundary
      corfu-preview-current t		; enable candidate preview
      corfu-echo-documentation t	; documentation in echo area
      corfu-cycle t)			; enable cycling for corfu-next and corfu-previous

(define-key corfu-map "?" #'minibuffer-completion-help)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map (kbd "<tab>") 'corfu-next)
(define-key corfu-map (kbd "<backtab>") 'corfu-previous)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)

(require 'recentf)
(recentf-mode 1)			; keybinding in keybindings section toward the end

(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))
(require 'eglot)

(setq eglot-autoreconnect t
      eglot-send-changes-idle-time 1
      eglot-confirm-server-initiated-edits nil
      eglot-extend-to-xref t)

(let ((pylspdir (expand-file-name "lsp/pylsp" user-emacs-directory)))
  (unless (file-directory-p pylspdir)
    (make-directory pylspdir t)
    (shell-command (concat "python3 -m venv " pylspdir))
    (shell-command (concat ". " pylspdir "/bin/activate && pip install -U pip python-lsp-server[all]"))))

(unless (package-installed-p 'pyvenv)
  (package-refresh-contents)
  (package-install 'pyvenv))
(pyvenv-mode 1)

(add-hook 'python-mode-hook
	  (progn
	    (with-eval-after-load 'eglot
	      (push '(python-mode "~/.cache/emacs/lsp/pylsp/bin/pylsp" "--verbose") eglot-server-programs))
	    'eglot-ensure))

(add-hook 'c-mode-hook 'eglot-ensure)

(let ((goplsdir (expand-file-name "lsp/gopls" user-emacs-directory)))
  (unless (file-directory-p goplsdir)
    (make-directory goplsdir t)
    (setenv "GOPATH" goplsdir)
    (shell-command "go install golang.org/x/tools/gopls@latest")))

(add-hook 'go-mode-hook
	  (progn
	    (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "lsp/gopls" user-emacs-directory)))
	    (setq exec-path (split-string (getenv "PATH") path-separator))
	    (with-eval-after-load 'eglot
	      (add-to-list 'eglot-server-programs '(go-mode "~/.cache/emacs/lsp/gopls/bin/gopls" "-verbose")))
	    'eglot-ensure))

(unless (package-installed-p 'go-mode)
  (package-refresh-contents)
  (package-install 'go-mode))

;;  The project package does not natively know about GOPATH or Go
;;  modules. Fortunately, you can give it a custom hook to tell it to
;;  look for the nearest parent go.mod file (that is, the root of the
;;  Go module) as the project root
(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)


;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(let ((jdtlsdir (expand-file-name "lsp/jdtls" user-emacs-directory)))
  (unless (file-directory-p jdtlsdir)
    (make-directory jdtlsdir t)
    (url-copy-file "https://download.eclipse.org/jdtls/milestones/1.16.0/jdt-language-server-1.16.0-202209291445.tar.gz" "/tmp/jdtls.tar.gz" t t)
    (shell-command (concat "tar -xzf /tmp/jdtls.tar.gz -C"  (expand-file-name "lsp/jdtls" user-emacs-directory)))))

(add-hook 'java-mode-hook
	  (progn
	    (setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/jdk-17/bin"))
	    (setenv "JAVA_HOME" "/usr/local/jdk-17")
	    (with-eval-after-load 'eglot
	      (add-to-list 'eglot-server-programs '(java-mode "~/.cache/emacs/lsp/jdtls/bin/jdtls" "-verbose")))
	    'eglot-ensure))

(setq eldoc-echo-area-display-truncation-message t
      eldoc-echo-area-use-multiline-p t
      eldoc-echo-area-prefer-doc-buffer t)

(unless (package-installed-p 'yasnippet)
  (package-refresh-contents)
  (package-install 'yasnippet)
  (package-install 'yasnippet-snippets))
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(unless (package-installed-p 'yaml-mode)
  (package-refresh-contents)
  (package-install 'yaml-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(unless (package-installed-p 'neotree)
  (package-refresh-contents)
  (package-install 'neotree))
(setq neo-smart-open t			; jump to current file open
      )

(unless (package-installed-p 'ytdl)
  (package-refresh-contents)
  (package-install 'ytdl))
(require 'ytdl)

(unless (package-installed-p 'ein)
  (package-refresh-contents)
  (package-install 'ein))

(defun my-move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(defun my-move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
;; both shortcuts can be emulated with C-x C-t C-x z z z... and C-1 C-x C-t z z z..
;; (global-set-key (kbd "<C-M-down>") 'my-move-line-down)
;; (global-set-key (kbd "<C-M-up>") 'my-move-line-up)

(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

(defun my-split-below (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
		  (window-parent (selected-window)))
		nil 'below nil))
(defun my-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
			  (not (window-dedicated-p (selected-window)))))

(defun my-rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't rotate a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while  (< i numWindows)
	   (let* (
		  (w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))

		  (s1 (window-start w1))
		  (s2 (window-start w2))
		  )
	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))
(defun my-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun my-repeat-command (command)
  "Repeat COMMAND."
  (require 'repeat)
  (let ((repeat-previous-repeated-command  command)
	(repeat-message-function           #'ignore)
	(last-repeatable-command           'repeat))
    (repeat nil)))
(defun my-other-window (&optional arg)
  "Repeatable version of other-window"
  (interactive "P")
  (my-repeat-command 'other-window))

;; (global-set-key (kbd "M-<SPC>") ctl-x-map)

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))

    ;; restore original mapping for M-SPC
    (define-key map (kbd "<SPC>") 'just-one-space)

    ;; Core Emacs Bindings
    (define-key map (kbd "a i") 'imenu)
    (define-key map (kbd "a t") 'neotree-toggle)
    (define-key map (kbd "a r") 'recentf-open-files)

    ;; grep
    (define-key map (kbd "g g") 'grep)
    (define-key map (kbd "g f") 'grep-find)
    (define-key map (kbd "g r") 'rgrep)	; interactive grep-find
    (define-key map (kbd "g z") 'zgrep)

    ;; lsp
    (define-key map (kbd "l a") 'eglot-code-actions)
    (define-key map (kbd "l e") 'eglot-events-buffer)
    (define-key map (kbd "l f") 'eglot-format)
    (define-key map (kbd "l r") 'eglot-rename)

    ;; embark
    (define-key map (kbd "e a") 'embark-act)
    (define-key map (kbd "e b") 'embark-become)

    ;; windows
    (define-key map (kbd "w b") 'my-split-below)
    (define-key map (kbd "w d") 'my-toggle-window-dedication)
    (define-key map (kbd "w o") 'my-other-window)
    (define-key map (kbd "w r") 'my-rotate-windows)
    (define-key map (kbd "w t") 'my-toggle-window-split)
    map))

(global-set-key (kbd "M-<SPC>") my-mode-map)
