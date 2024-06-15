;;; initialization

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))


(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  (expand-file-name (convert-standard-filename "eln-cache/")
			    user-emacs-directory)))


(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(require 'package)
(if (< emacs-major-version 28)
    (add-to-list 'package-archives
		 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(add-to-list 'package-archives
	     '("melpa" . "https://stable.melpa.org/packages/") t)
(package-initialize)			; make sure to update load-path for downloaded packages

(org-babel-load-file "~/.config/emacs/myinit.org")
