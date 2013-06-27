;; cdk-config.el - Contains my customizations and configurations
;; Last modified : Thu, 27 June 2013 01:11:06 PDT

;; User details

(setq user-full-name "Chinmay Kamat")
(setq user-mail-address "chinmaykamat@gmail.com")

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

(provide 'cdk-config)
