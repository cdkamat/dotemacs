;; mode-config.el - contains requires and mode settings

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

;; Auto-complete mode
(require 'auto-comp-config)
(require 'auto-complete-latex nil 'noerror)
(setq ac-l-dict-directory (concat el-get-dir "/auto-complete-latex/ac-l-dict/"))
(add-hook 'latex-mode-hook 'ac-l-setup)

;; Magit
(require 'magit nil 'noerror)
(require 'magit-blame nil 'noerror)
(setq magit-last-seen-setup-instructions "1.4.0")

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

(require 'google-c-style)
(require 'prog-mode-config)

(provide 'mode-config)
