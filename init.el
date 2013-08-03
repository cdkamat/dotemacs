;; init.el Loads everything
;; Last modified : Sat, 3 August 2013 16:20:19 PDT

;;Emacs load path
(defvar cdk-lisp-dir "~/.emacs.d")

(let* ((default-directory cdk-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons cdk-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; CDK customizations
(require 'cdk-config)

;;Custom functions and key bindings
(require 'utility-functions)

;; Mode configurations
(require 'mode-config)

;;Custom keybindings
(require 'misc-bindings)
