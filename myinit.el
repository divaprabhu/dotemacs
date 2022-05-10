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

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(package-initialize)
(if (< emacs-major-version 28)
    (add-to-list 'package-archives
		 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

;; (add-to-list 'package-archives
;;		 '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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
	 (side . bottom)
	 (window-height . 0.4)
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
(setq flymake-no-changes-timeout nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 1)

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

;; don't create newsrc file that other clients may use
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)

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

(add-hook 'python-mode-hook
	  (progn
	    (setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/venv/bin"))
	    (setq exec-path (split-string (getenv "PATH") path-separator))
	    (with-eval-after-load 'eglot
	      (push '(python-mode "~/venv/bin/pylsp" "--verbose") eglot-server-programs))
	    'eglot-ensure))

(add-hook 'c-mode-hook 'eglot-ensure)

(unless (package-installed-p 'go-mode)
  (package-refresh-contents)
  (package-install 'go-mode))

(add-hook 'go-mode-hook
	  (progn
	    (setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/go/bin"))
	    (setq exec-path (split-string (getenv "PATH") path-separator))
	    (with-eval-after-load 'eglot
	      (push '(go-mode "~/go/bin/gopls" "-verbose") eglot-server-programs))
	    'eglot-ensure))

(setq eldoc-echo-area-display-truncation-message t
      eldoc-echo-area-use-multiline-p t
      eldoc-echo-area-prefer-doc-buffer t)

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

    (define-key map (kbd "o")   'my-other-window)
      map))

  (global-set-key (kbd "M-<SPC>") my-mode-map)
