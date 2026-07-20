;;; init.el  --- Init  -*- lexical-binding: t; -*-

;; Redirect Emacs's generated/cached files to ~/.cache/emacs/ to keep ~/.config/emacs/ clean
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
;; Put ELPA packages into ~/.cache/emacs/elpa/
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
;; Put natively-compiled .eln files into ~/.cache/emacs/eln-cache/
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
	  (expand-file-name (convert-standard-filename "eln-cache/")
			    user-emacs-directory)))

;; Set up package system — add MELPA and initialise
(require 'package)
;; (add-to-list 'package-archives
             ;; '("melpa" . "https://melpa.org/packages/") t)
;; (setq package-archive-priorities
      ;; '(("gnu"    . 20)
        ;; ("nongnu" . 10)
        ;; ("melpa"  . 0)))
(package-initialize)

;; Configure auth-source to read encrypted credentials from ~/.gnupg/
(require 'auth-source)
(setq auth-sources '("~/.gnupg/authinfo.gpg" "~/.gnupg/authinfo" "~/.gnupg/netrc"))

;; Tangle myinit.org → myinit.el (only if .el missing) then load it
(let* ((org-file (expand-file-name "myinit.org" "~/.config/emacs/"))
       (el-file  (concat (file-name-sans-extension org-file) ".el")))
  (unless (file-exists-p el-file)
    (require 'ob-tangle)
    (org-babel-tangle-file org-file el-file))
  (load el-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
