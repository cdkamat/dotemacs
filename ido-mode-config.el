;; ido-mode-config.el --  Configuration for ido-mode

(setq 
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-case-fold  t
  ido-enable-last-directory-history t
  ido-max-work-directory-list 30
  ido-max-work-file-list      50
  ido-use-filename-at-point nil
  ido-use-url-at-point nil
  ido-enable-flex-matching t
  ido-max-prospects 8
  ido-confirm-unique-completion t
  ido-create-new-buffer 'always
  ido-enable-tramp-completion t
  ;; do not ask for confirmation
  confirm-nonexistent-file-or-buffer nil)

(ido-mode t)
(ido-everywhere t)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

(provide 'ido-mode-config)

;; ido-mode-config.el ends here
