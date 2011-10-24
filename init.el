;; init.el Loads everything
;; Last modified : Sun, 23 October 2011 20:03:29 EDT

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

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
