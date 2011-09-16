;;Functions I use

(require 'thingatpt)
(require 'imenu)

;;From emacs-starter-kit http://github.com/technomancy/emacs-starter-kit/
(defun esk-flatten-assoc-tree (tree pred)
  "Returns an alist of only (key . leaf) pairs in TREE. PRED
determines whether a value is a sub-alist or a leaf."
  (flet ((inner (lst)
                (mapcan (lambda (elt)
                          (cond ((atom elt) nil)
                                ((funcall pred elt) (inner elt))
                                (t (list elt))))
                        lst)))
    (inner tree)))

(defun ido-imenu ()
  "Queries with `ido-completing-read' a symbol in the buffer's
imenu index, then jumps to that symbol's location."
  (interactive)
  (goto-char
   (let ((lst (nreverse (esk-flatten-assoc-tree
                         (imenu--make-index-alist) 'imenu--subalist-p))))
     (cdr (assoc (ido-completing-read "Symbol: " (mapcar 'car lst)) lst)))))

;;settings for hippie-expand
(setq hippie-expand-try-functions-list
       '(try-expand-dabbrev
         try-expand-dabbrev-from-kill
         try-expand-dabbrev-all-buffers
         try-complete-file-name-partially
         try-complete-file-name
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

;;function to implement a smarter TAB
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
  the minibuffer. Else, if mark is active, indents region. Else if
  point is at the end of a symbol, expands it. Else indents the
  current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(hippie-expand nil))
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (auto-complete)
	(indent-for-tab-command)))))
(global-set-key (kbd "TAB") 'smart-tab)

;;Some C mode hooks
(add-hook 'c-mode-common-hook 
	  (lambda ()
	    (c-set-style "linux")
	    (setq c-basic-offset 4)
	    (setq-default indent-tabs-mode nil)))

;; I-search with initial contents
(defvar isearch-initial-string "")

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (isearch-yank-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
	(setq isearch-initial-string (buffer-substring-no-properties begin end))
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

;;Programming mode settings -- taken from http://github.com/vedang/emacs.d
(defvar programming-major-modes
  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode conf-mode)
  "List of programming modes")

(defun prog-mode-settings ()
  "special settings for programming modes."
  (when (memq major-mode programming-major-modes)
    ;;No stray edits.Toggle with (C-x C-q) if I want to make an edit
    (toggle-read-only 1)
    (which-function-mode t)
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)
    (setq comment-style 'extra-line)
    (setq comment-multi-line 't)
    ;;Flyspell mode for comments and strings
    (flyspell-prog-mode)
    ;; This highlights part of line > 80 with an ugly pink color
    ;; Remove text having this color
    (setq whitespace-style '(lines-tail trailing))
    (whitespace-mode)))
(add-hook 'find-file-hook 'prog-mode-settings)

;;Force backup of buffer after each save
(defun force-backup-of-buffer ()
    (setq buffer-backed-up nil))

(defun sudo-edit (&optional arg)
  "Edit as root"
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun backward-kill-word-or-kill-region (&optional arg)
  "Change C-w behavior"
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

;; Column fill settings for text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (auto-fill-mode)
	    (setq fill-column 80)))

(setq whitespace-style '(trailing))
(whitespace-mode)

(provide 'utility-functions)
