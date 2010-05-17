;; User details

(setq user-full-name "Chinmay Kamat")
(setq user-mail-address "chinmaykamat@gmail.com")

;;Emacs load path
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; no scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; no tool-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; no menu-bar
;;use y-or-n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode 1) ;; Colours
(show-paren-mode 1) ;;Show paren matching 
(setq inhibit-startup-screen 1)
(delete-selection-mode 1);;copy overwrite selected
(setq-default ispell-program-name "aspell");;spell checker
(setq require-final-newline t)
(setq debug-on-error t)
(setq visible-bell t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;Emacs back up settings
(push '("." . "~/.emacs.d/.emacs-backups/") backup-directory-alist)
(setq version-control t
      kept-new-versions 15
      kept-old-versions 5
      delete-old-versions t
      backup-by-copying-when-linked t
      vc-make-backup-files t);;Make backup even if under version control
(add-hook 'before-save-hook  'force-backup-of-buffer)

;;Custom keybindings
(require 'misc-bindings)

(require 'saveplace)
(setq-default save-place t)
(require 'recentf)

;;Custom functions and key bindings
(require 'utility-functions)

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
(require 'yasnippet) ;; not yasnippet-bundle
(global-set-key (kbd "S-TAB") 'yas/trigger-key)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;;Cscope
(require 'cs-bindings)

;;Elisp mode
(require 'emacs-lisp-mode-config)

;;Ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

;;Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;Bitkeeper
(require 'bk)

;; Color theme
(require 'color-theme-wombat)
(require 'folio)
(if window-system
    (color-theme-wombat))

;; PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
			       auto-mode-alist))
