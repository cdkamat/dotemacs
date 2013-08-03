;; mode-config.el - contains requires and mode settings
;; Last modified : Sat, 3 August 2013 15:32:29 PDT

;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/themes")
(load-theme 'cdk-wombat t)
(set-frame-font "Ubuntu Mono-12")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))

;; Setting for Mac OS X
(if (eq system-type 'darwin)
    (progn
      (set-frame-font "Monaco-12")
      (add-to-list 'default-frame-alist '(font . "Monaco-12"))))

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
(yas-global-mode)
(global-set-key (kbd "<s-tab>") 'yas/expand)
(yas-load-directory "~/.emacs.d/plugins/el-get/yasnippet/snippets")

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
(require 'auto-complete-latex nil 'noerror)
(add-hook 'latex-mode-hook 'ac-l-setup)

;; Magit
(require 'magit nil 'noerror)
(require 'magit-blame nil 'noerror)

;; Enable elpy for python
(require 'elpy nil 'noerror)
(elpy-enable)

;; Proto buf mode
(require 'protobuf-mode nil 'noerror)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; exec-path-from-shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Column fill settings for text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq fill-column 80)
            (turn-on-auto-fill)))

;;Some C mode hooks
(require 'google-c-style)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "linux")
	    (setq c-basic-offset 4)
            (local-set-key (kbd "C-c C-c") 'compile)
            (let ((filename (buffer-file-name)))
              ;; Enable google-style for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/nutanix")
                                       filename))
                (google-set-c-style)))))


(add-hook 'asm-mode-hook
          (lambda ()
            (local-set-key (kbd "<f8>") 'toggle-asm-comment-char)
            (custom-set-variables
             '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                                       64 68 72 76 80 84 88 92 96 100 104 108
                                       112 116 120))))))

;;Programming mode settings -- taken from http://github.com/vedang/emacs.d
(defvar programming-major-modes
  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode java-mode
                    conf-mode asm-mode python-mode)
  "List of programming modes")

(defun prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode programming-major-modes)
    ;;No stray edits.Toggle with (C-x C-q) if I want to make an edit
    (if (not (equal major-mode 'emacs-lisp-mode))
      (read-only-mode 1))
    (which-function-mode t)
    ;; Never use tabs to indent in prog-modes
    (setq-default indent-tabs-mode nil)
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (local-set-key (kbd "RET") 'newline-and-indent)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (hs-minor-mode t)
    (setq comment-style 'extra-line)
    (setq comment-multi-line 't)
    ;;Flyspell mode for comments and strings
    (flyspell-prog-mode)
    ;; This highlights lines > 80 with an ugly pink color
    ;; Remove text having this color
    (setq whitespace-style '(face lines trailing))
    (whitespace-mode)
    (setq fill-column 80)
    (turn-on-auto-fill)
    (setq comment-auto-fill-only-comments t)
    (semantic-mode 1)
    (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))
    (font-lock-add-keywords nil
          '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(add-hook 'find-file-hook 'prog-mode-settings)

(provide 'mode-config)
