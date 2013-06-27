;; init.el Loads everything
;; Last modified : Thu, 27 June 2013 01:29:36 PDT

;;Emacs load path
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; el-get bootstrap
(unless (require 'el-get nil t)
  (setq el-get-install-branch "master")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (eval-print-last-sexp))
  (el-get-emacswiki-refresh el-get-recipe-path-emacswiki t))

(setq el-get-dir "~/.emacs.d/plugins/el-get/")

;; Lines below this should be commented out till all the el-get packages as
;; described in the README have been installed.

;; CDK customizations
(require 'cdk-config)

;;Custom functions and key bindings
(require 'utility-functions)

;; Mode configurations
(require 'mode-config)

;;Custom keybindings
(require 'misc-bindings)
