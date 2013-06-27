;; utility-functions.el - contains the functions that I use
;; Last modified : Tue, 3 July 2012 16:08:09 PDT

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

(defun toggle-asm-comment-char ()
    "Toggle asm comment char in ; and @"
    (interactive)
    (setq asm-comment-char (if (= asm-comment-char 59) 64 59))
    (revert-buffer nil 1))

;;Force backup of buffer after each save
(defun force-backup-of-buffer ()
    (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-backup-of-buffer)

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

(provide 'utility-functions)
