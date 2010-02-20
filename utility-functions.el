;function to implement a smarter TAB
(global-set-key (kbd "TAB") 'smart-tab)
(defun smart-tab ()
     "This smart tab is minibuffer compliant: it acts as usual in
       the minibuffer. Else, if mark is active, indents region. Else if
       point is at the end of a symbol, expands it. Else indents the
       current line."
     (interactive)
     (if (string-match "Minibuf" (buffer-name))
         (unless (minibuffer-complete)
           (dabbrev-expand nil))
       (if (looking-at "\\>")
	   (dabbrev-expand nil)
	 (indent-for-tab-command))))

;;Some C mode hooks
(add-hook 'c-mode-common-hook 
  (lambda ()
    (which-function-mode t)
    (set-face-background 'which-func "white")
    (set-face-foreground 'which-func "blue")
    (global-set-key "\M-gc" 'complete-tag)
    (setq-default indent-tabs-mode nil)
    (setq c-basic-offset 4)))

;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(define-key global-map "\M-s" 'isearch-forward-at-point)

;;Insert current buffer name in minibuf on F3
(define-key minibuffer-local-map
  [f3] (lambda () (interactive) 
	 (insert (buffer-name (current-buffer-not-mini)))))

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
          (window-buffer (previous-window)) (window-buffer (next-window)))))

;;function to Copy-only instead of kill (reddit comments)
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key (kbd "C-c C-k") 'copy-line)

;;Hideshow
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;;A better comment function
(defun my-comment-line-or-region ()
  (interactive "*")
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) nil)
    (progn
      (save-excursion
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (move-end-of-line nil)
        (comment-dwim nil)
        ))))
(global-set-key (read-kbd-macro "M-;") 'my-comment-line-or-region)

;;Programming mode settings -- taken from http://github/vedang/emacs.d
(defvar programming-major-modes
  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode conf-mode)
  "List of programming modes")

(defun prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode programming-major-modes)
    ;;No stray edits.Toggle with (C-x C-q) if I want to make an edit
    (toggle-read-only 1)
    ;;Flyspell mode for comments and strings
    (flyspell-prog-mode)))

;;Some keybindings
(global-set-key [f1] 'manual-entry) ;; Man pages
(define-key global-map "\M-r" 'query-replace-regexp) ;; replace Regex

;;Keybindings for clipboard cut-copy-paste
;;Works better when working with terminal mode
(global-set-key [kbd (shift delete)] 'clipboard-kill-region)
(global-set-key [kbd (control insert)] 'clipboard-kill-ring-save)
(global-set-key [kbd (shift insert)] 'clipboard-yank)

(provide 'utility-functions)