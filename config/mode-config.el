;; mode-config.el - contains requires and mode settings
;; Last modified : Sat, 5 November 2011 20:00:13 EDT

;; Color theme
(require 'color-theme-wombat)
(require 'color-theme-zenburn)
;;   (color-theme-zenburn)
(color-theme-wombat)
(add-hook 'after-make-frame-functions 'color-theme-wombat)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat cache-dir "saveplace"))

;;Org mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-agenda-files (file-expand-wildcards "~/Documents/Notes/*.org"
                         "~/Documents/Notes/archive/*.org_archive"))
(require 'org-mode-config)
(org-agenda-to-appt)

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
(require 'ido-mode-config)

;;Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'server)
;; (setq server-use-tcp t)
;; (setq server-host "serenity")
;; ;; Suppress error "directory
;; ;; ~/.emacs.d/server is unsafe"
;; (when (= emacs-major-version 23)
;;   (defun server-ensure-safe-dir (dir) "Noop" t))
;; (setq server-socket-dir "~/.emacs.d/")

(icomplete-mode)
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;; ibuffer mode settings
(setq ibuffer-default-sorting-mode 'major-mode)

(require 'latex-mode-config)

(require 'tbemail)

;; Auto-complete mode
(require 'auto-comp-config)

;; Magit
(require 'magit)

;; Column fill settings for text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq fill-column 80)
            (turn-on-auto-fill)))
;;Some C mode hooks
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "linux")
	    (setq c-basic-offset 4)
            (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'asm-mode-hook
          (lambda ()
            (local-set-key (kbd "<f8>") 'toggle-asm-comment-char)))

;;Programming mode settings -- taken from http://github.com/vedang/emacs.d
(defvar programming-major-modes
  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                    conf-mode asm-mode makefile-mode)
  "List of programming modes")

(defun prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode programming-major-modes)
    ;;No stray edits.Toggle with (C-x C-q) if I want to make an edit
    (toggle-read-only 1)
    (which-function-mode t)
    ;; Never use tabs to indent in prog-modes
    (setq-default indent-tabs-mode nil)
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)
    (setq comment-style 'extra-line)
    (setq comment-multi-line 't)
    ;;Flyspell mode for comments and strings
    (flyspell-prog-mode)
    ;; This highlights lines > 80 with an ugly pink color
    ;; Remove text having this color
    (setq whitespace-style '(lines trailing))
    (whitespace-mode)
    (setq fill-column 80)
    (turn-on-auto-fill)
    (setq comment-auto-fill-only-comments t)
    (font-lock-add-keywords nil
          '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(add-hook 'find-file-hook 'prog-mode-settings)

(provide 'mode-config)

