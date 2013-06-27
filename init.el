;; init.el Loads everything
;; Last modified : Wed, 26 June 2013 17:31:00 PDT

;;Emacs load path
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; CDK customizations
(require 'cdk-config)

;;Custom functions and key bindings
(require 'utility-functions)

;; Mode confiturations
(require 'mode-config)

;;Custom keybindings
(require 'misc-bindings)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
