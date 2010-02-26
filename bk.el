;;Functions to execute Bit keeper commands from boxer

(require 'vc)

(defun bk-edit ()
  "Function to run bk edit on the current buffer"
  (interactive)
  (shell-command (concat "bk edit " 
                         (buffer-file-name (current-buffer))))
  (revert-buffer t t))

(defun bk-unedit ()
  "Function to run bk unedit on the current buffer.
Confirm before execution"
  (interactive)
  (if (y-or-n-p (format "Do you want run unedit on %s" (buffer-name (current-buffer))))
      (shell-command (concat "bk unedit " 
                         (buffer-file-name (current-buffer)))))
  (revert-buffer t t))

(defun bk-diffs ()
  "Function to run bk diffs on current file."
  (interactive)
  (shell-command (concat "bk diffs -u " 
                         (buffer-file-name (current-buffer)))
		 (pop-to-buffer 
		  (concat "bk-diff for file " (buffer-name (current-buffer))) t))
  (diff-mode))

;; Keybindings only for BK mode
;; TODO -- change this to work only for bk/sccs - not for all vc modes
(defun bk-mode-settings ()
  "special settings for programming modes."
  (if vc-mode 
      (progn 
      (local-set-key [f6] 'bk-edit)
      (local-set-key [f7] 'bk-unedit)
      (local-set-key [f8] 'bk-diffs)))) 
(add-hook 'find-file-hook 'bk-mode-settings t)

(provide 'bk)
