;;; initialization

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
(org-babel-load-file "~/.config/emacs/myinit.org")
