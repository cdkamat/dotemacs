(defun latex-mode-settings ()
  "my settings for LaTeX mode"
  ;always have reftex along with auctex
  (turn-on-reftex)
  (flyspell-mode))

(add-hook 'LaTeX-mode-hook 'latex-mode-settings)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;turn on pdf-mode.  AUCTeX
                                          ;will call pdflatex to
                                          ;compile instead of latex.
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t)

(provide 'latex-mode-config)
