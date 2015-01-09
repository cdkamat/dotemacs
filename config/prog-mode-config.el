;; prog-mode-config.el - contains config for programming modes

;; C mode settings
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "linux")
	    (setq c-basic-offset 4)
            (local-set-key (kbd "C-c C-c") 'compile)
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (local-set-key (kbd "C-c j") 'semantic-complete-jump)
            (local-set-key (kbd "C-c d") 'semantic-ia-fast-jump)
            (local-set-key (kbd "M-]")   'semantic-symref-symbol)
            (let ((filename (buffer-file-name)))
              ;; Enable google-style for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/nutanix")
                                       filename))
                (google-set-c-style)))))

;; Python mode settings
(add-hook 'python-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              (when (and filename
                         (string-match (expand-file-name "~/src/nutanix")
                                       filename))
                (setq indent-tabs-mode nil
                      python-indent-offset 2)))))

;; ASM mode settings
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
                    conf-mode asm-mode python-mode protobuf-mode)
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
    (setq semanticdb-project-roots (quote ("~/src/nutanix/main/"))
          semantic-idle-work-parse-neighboring-files-flag nil)
    (font-lock-add-keywords nil
          '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

(add-hook 'find-file-hook 'prog-mode-settings)

(provide 'prog-mode-config)
