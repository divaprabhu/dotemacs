;;; init.el  --- Init  -*- lexical-binding: t; -*-

;; Avoid littering
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  (expand-file-name (convert-standard-filename "eln-cache/")
			    user-emacs-directory)))

;; Initialize Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)			; make sure to update load-path for downloaded packages

(require 'auth-source)
(setq auth-sources '("~/.gnupg/authinfo.gpg" "~/.gnupg/authinfo" "~/.gnupg/netrc"))

;;   (org-babel-load-file "~/.config/emacs/myinit.org")

(let* ((org-file (expand-file-name "myinit.org" "~/.config/emacs/"))
       (el-file  (concat (file-name-sans-extension org-file) ".el")))
  (unless (file-exists-p el-file)
    (require 'ob-tangle)
    (org-babel-tangle-file org-file el-file))
  (load el-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
