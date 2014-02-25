;;; cdk-wombat-theme.el --- Wombat color theme modified as per my requirements
;; Chinmay Kamat (@cdkamat)
;;
;; File structure based on tomorrow-night-theme.el
;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles
;; Ported to GNU Emacs 24's built-in theme system by Jim Myhrberg (@jimeh)

;; Last modified: Mon, 24 February 2014 18:09:20 PST

;;; Code:

(deftheme cdk-wombat
  "A Pastel Coloured Theme")

(let ((background "#1d1f21")
      (current-line "#282a2e")
      (selection "#373b41")
      (foreground "#f6f3e8")
      (comment "#969896")
      (cursor "#8ac6f2")
      (red "#cc6666")
      (warning-red "#ff0000")
      (orange "#de935f")
      (yellow "#f0dfaf")
      (green "#95e454")
      (aqua "#8abeb7")
      (blue "#8ac6f2")
      (purple "#b294bb"))

  (custom-theme-set-faces
   'cdk-wombat

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,current-line))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,current-line :foreground ,foreground))))
   `(region ((t (:background ,selection))))
   `(cursor ((t (:background ,cursor))))
   ;; Font-lock stuff
   `(font-lock-comment-face ((t (:italic t :foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,red))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:italic t :bold t :foreground ,purple))))
   `(font-lock-keyword-face ((t (:bold t :foreground ,blue))))
   `(font-lock-preprocessor-face ((t (:foreground ,red))))
   `(font-lock-string-face ((t (:italic t :foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,foreground))))
   `(font-lock-warning-face ((t (:bold t :foreground ,warning-red
                                       :background ,yellow))))

   ;; which-func mode
   `(which-func-face ((t (:foreground ,foreground))))
   `(which-func ((t (:foreground ,foreground))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,current-line :foreground ,foreground))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,current-line))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))
   `(org-warning ((t (:inherit nil))))

   ;; diff-mode
   `(diff-added ((t (:foreground ,green :background nil))))
   `(diff-removed ((t (:foreground ,red :background nil))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line)))))

  (custom-theme-set-variables
   'cdk-wombat

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'cdk-wombat)

;;; cdk-wombat-theme.el ends here
