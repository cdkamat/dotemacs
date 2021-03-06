;; init.el Loads everything

;;Emacs load path
(add-to-list 'load-path "~/.emacs.d/config/")

(let ((default-directory "~/.emacs.d/plugins/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (normal-top-level-add-subdirs-to-load-path))
         load-path)))

;; CDK customizations
(require 'cdk-config)

;;Custom functions and key bindings
(require 'utility-functions)

;; Mode configurations
(require 'mode-config)

;;Custom keybindings
(require 'misc-bindings)
