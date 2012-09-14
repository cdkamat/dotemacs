;; init.el Loads everything
;; Last modified : Fri, 14 September 2012 02:13:48 EDT

;;Emacs load path
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; CDK customizations
(require 'cdk-config)

;; Mode confiturations
(require 'mode-config)

;;Custom functions and key bindings
(require 'utility-functions)

;;Custom keybindings
(require 'misc-bindings)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
