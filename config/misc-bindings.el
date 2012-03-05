;; misc-bindings.el - contains key binding
;; Last modified : Mon, 5 March 2012 11:58:24 EST

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;Ido Imenu
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;;Some keybindings
(global-set-key [f1] 'woman) ;; Man pages
(define-key global-map "\M-r" 'query-replace-regexp) ;; replace Regex
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-y") 'yank-pop)

;;Keybindings for clipboard cut-copy-paste
;;Works better when working with terminal mode
(global-set-key [kbd (shift delete)] 'clipboard-kill-region)
(global-set-key [kbd (control insert)] 'clipboard-kill-ring-save)
(global-set-key [kbd (shift insert)] 'clipboard-yank)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; Keybinding for functions defined in utility-functions.el

(global-set-key (kbd "TAB") 'smart-tab)
(define-key global-map "\M-s" 'isearch-forward-at-point)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
;;Insert current buffer name in minibuf on F3
(global-set-key [f3] (lambda () (interactive)
	 (insert (buffer-name (current-buffer-not-mini)))))

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

(provide 'misc-bindings)
