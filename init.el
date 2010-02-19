(add-to-list 'load-path "~/.emacs.d/") ;;Emacs load path

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; no scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; no tool-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; no menu-bar
;;use y-or-n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode 1) ;; Colours
(show-paren-mode 1) ;;Show paren matching 
(setq inhibit-startup-screen 1)
(delete-selection-mode 1);;copy overwrite selected

;;Emacs back up settings
(push '("." . "~/.emacs.d/.emacs-backups/") backup-directory-alist)
(setq version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      backup-by-copying-when-linked t)

;;Custom functions and key bindings
(require 'custom)

;;Org mode 
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;settings for shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)
 
;;settings for yasnippet
(require 'yasnippet-bundle) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)

;;Cscope
(require 'cs_bindings)

;;Elisp mode
(require 'emacs-lisp-mode-config)

;;Color-themes
(require 'color-theme)

;;Ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;;Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)