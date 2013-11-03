;; cdk-config.el - Contains my customizations and configurations
;; Last modified : Sun, 3 November 2013 00:02:06 PDT

;; User details

(setq user-full-name "Chinmay Kamat")
(setq user-mail-address "chinmaykamat@gmail.com")

;; Boot-strapping the config
(unless (file-exists-p (concat cdk-lisp-dir "cache"))
  (make-directory (concat cdk-lisp-dir "cache") t))
(unless (file-exists-p (concat cdk-lisp-dir "plugins/el-get"))
  (make-directory (concat cdk-lisp-dir "plugins/el-get") t))

(setq el-get-dir (concat cdk-lisp-dir "plugins/el-get"))

;; el-get bootstrap
(unless (require 'el-get nil t)
  (setq el-get-install-branch "master")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp))
  (el-get-emacswiki-refresh el-get-recipe-path-emacswiki t))

;; install all necessary packages
(defvar cdk-el-get-packages)
(setq cdk-el-get-packages
      (append
       '(magit auto-complete auto-complete-latex yasnippet icomplete+
               elpy protobuf-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync cdk-el-get-packages)

;; UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; no scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; no tool-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; no menu-bar
(show-paren-mode 1) ;;Show paren matching
(setq font-lock-maximum-decoration t)
(delete-selection-mode 1);;copy overwrite selected

;; Misc settings
;; use y-or-n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default ispell-program-name "aspell");;spell checker
(setq inhibit-startup-screen 1
      require-final-newline t
      ;; setq debug-on-error t
      visible-bell t
      doc-view-continuous t
      column-number-mode t
      x-select-enable-clipboard t
      xterm-mouse-mode t
      time-stamp-active 1
      time-stamp-line-limit 10
      time-stamp-pattern "\\([Ll]ast [Mm]odified\\|Date\\|@date\\) *[:]* %3a, %:d %:b %:Y %02H:%02M:%02S %Z$")

;; Mac OS X specific settings

(if (eq system-type 'darwin)
    (progn
      (setq visible-bell nil)
      (setq ring-bell-function #'ignore)))

;; Everying thing in utf-8
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8
      coding-system-for-write 'utf-8
      default-process-coding-system '(utf-8 . utf-8))
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; temp dir
(setq cache-dir "~/.emacs.d/cache/")

;;Emacs back up settings
(push '("." . "~/.emacs.d/.emacs-backups/") backup-directory-alist)
(setq version-control t
      kept-new-versions 15
      kept-old-versions 5
      delete-old-versions t
      backup-by-copying-when-linked t
      vc-make-backup-files t);;Make backup even if under version control

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'time-stamp)

;; Whitespace settings
(setq whitespace-style '(face trailing))
(whitespace-mode)
(defadvice terminal-init-xterm (after select-shift-up activate)
      (define-key input-decode-map "\e[1;2A" [S-up]))

(provide 'cdk-config)
