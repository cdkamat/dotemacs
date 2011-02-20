;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;Ido Imenu
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
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
(global-set-key [f1] 'manual-entry) ;; Man pages
(define-key global-map "\M-r" 'query-replace-regexp) ;; replace Regex
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-y") 'yank-pop)

;;Keybindings for clipboard cut-copy-paste
;;Works better when working with terminal mode
(global-set-key [kbd (shift delete)] 'clipboard-kill-region)
(global-set-key [kbd (control insert)] 'clipboard-kill-ring-save)
(global-set-key [kbd (shift insert)] 'clipboard-yank)

;; Add backward-kill-word as Yegge
(global-set-key (kbd "C-w") 'backward-kill-word) ;; easy editing
(global-set-key (kbd "C-x C-k") 'kill-region) ;; remapping C-w
(global-set-key (kbd "C-k") 'kill-line) ;; kill-line

(provide 'misc-bindings)
