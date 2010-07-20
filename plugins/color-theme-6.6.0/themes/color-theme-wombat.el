;;; color-theme-wombat.el --- The wombat color theme for Emacs.
;; Copyright (C) 2009 Jesus Alvarez

;; Author: Jesus Alvarez <demizer.one@gmail.com>
;; URL: n/a
;; Updated:

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is my second color theme. Apologies for the formatting; this
;; file is automatically extracted from a LaTeX master file.

;; CDK -- This is the wombat color scheme taken from
;; http://github.com/demizer/color-theme-wombat with my own modifications
;; The modifications include many settings from Zenburn
;; http://www.brockman.se/2003/zenburn/zenburn.el
;; WORK IN PROGRESS

;;; Code:
(require 'color-theme)

(defvar wombat-fg "#f6f3e8")
(defvar wombat-bg "#242424")
(defvar wombat-green "#95e454")
(defvar wombat-green+1 "#cae682")
(defvar wombat-green+2 "#4BC98A")
(defvar wombat-red-1 "#e5786d")
(defvar wombat-red "#95e454")
(defvar wombat-blue-2 "#2e3436")
(defvar wombat-blue-1 "#64a8d8")
(defvar wombat-blue "#8ac6f2")
(defvar wombat-magenta "#cc99cc")
(defvar wombat-orange-1 "#f57900")
(defvar wombat-orange "#e65c00")
(defvar wombat-orange+1 "#e9b96e")
(defvar wombat-orange+2 "#ffc125")
(defvar wombat-purple-1 "#ad7fa8")
(defvar wombat-purple "#cc99cc")
(defvar wombat-pink-1 "#f283b6")
(defvar wombat-pink "#F6B3DF")
(defvar wombat-gray-1 "#444444")
(defvar wombat-gray "#424242")
(defvar wombat-gray+1 "#99968b")

(defun color-theme-wombat (&optional frame)
  "The wombat color theme for Emacs."
  (interactive)
  ;; set font 
  (set-frame-font "Consolas-13")
  (color-theme-install
   `(color-theme-wombat
     ((background-color . ,wombat-bg)
      (background-mode . dark)
      (border-color . ,wombat-bg)
      (cursor-color . ,wombat-blue-1)
      (foreground-color . ,wombat-fg)
      (mouse-color . "white"))

     ;; Font Lock
     (font-lock-builtin-face ((t (:foreground ,wombat-blue))))
     (font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground ,wombat-gray+1))))
     (font-lock-comment-face ((t (:italic t :slant italic :foreground ,wombat-gray+1))))
     (font-lock-constant-face ((t (:foreground ,wombat-red-1))))
     (font-lock-doc-face ((t (:foreground ,wombat-gray+1))))
     (font-lock-function-name-face ((t (:foreground ,wombat-purple-1 :bold t :italic t))))
     (font-lock-keyword-face ((t (:bold t :weight bold :foreground ,wombat-blue))))
     (font-lock-negation-char-face ((t (:foreground ,wombat-red))))
     (font-lock-preprocessor-face ((t (:foreground ,wombat-red-1))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t ,wombat-green))))
     (font-lock-string-face ((t (:italic t :foreground ,wombat-green))))
     (font-lock-type-face ((t (:foreground "#f0dfaf"))))
     (font-lock-variable-name-face ((t (:foreground ,wombat-fg))))
     (font-lock-warning-face ((t (:bold t :foreground ,wombat-red))))
     
     (which-func-face ((t (:foreground "white"))))
     (which-func ((t (:foreground "white"))))

     ;; UI Items
     ;(border ((t (:background "#888a85"))))
     ;(fringe ((t (:background "grey10"))))
     (minibuffer-prompt ((t (:foreground ,wombat-red :bold t))))
     (mode-line ((t (:background ,wombat-gray-1 :foreground ,wombat-fg))))
     (mode-line-emphasis ((t (:bold t))))
     (mode-line-highlight ((t (:background ,wombat-orange :box nil))))
     (mode-line-inactive ((t (:background ,wombat-bg :box (:line-width 1 :color ,wombat-gray :style nil)))))
     (region ((t (:foreground ,wombat-fg :background ,wombat-gray-1))))


     ;; Highlighting
     (lazy-highlight ((t (:italic t :background "yellow" :foreground "black"))))
     (highlight ((t (:background ,wombat-gray-1))))
     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     (highlight-changes-face ((t (:foreground "red"))))
     (secondary-selection ((t (:background ,wombat-blue-1 :foreground "black" :bold t))))
     (hl-line ((t (:background ,wombat-gray-1))))


     ;; Org-mode
     (org-hide ((t (:foreground ,wombat-bg))))
     (org-agenda-date-today ((t (:foreground "white" 
                               :slant italic :weight bold))) t)      
     (org-agenda-structure ((t (:inherit font-lock-comment-face))))  
     (org-archived ((t (:foreground "#8f8f8f"))))                    
     (org-column ((t (:height 98 :family "DejaVu Sans Mono"))))      
     (org-checkbox ((t (:background "#5f5f5f" :foreground "white"    
                      :box (:line-width 1 :style released-button)))))
     (org-date ((t (:foreground "#8cd0d3" :underline t))))           
     (org-deadline-announce ((t (:foreground "#bc8383"))))           
     (org-done ((t (:bold t :weight bold :foreground "#afd8af"))))   
     (org-formula ((t (:foreground "#d0bf8f"))))                     
     (org-headline-done ((t (:foreground "#afd8af"))))               
     (org-level-1 ((t (:foreground "#dfaf8f"))))                     
     (org-level-2 ((t (:foreground "#f0dfaf"))))                     
     (org-level-3 ((t (:foreground "#8cd0d3"))))                     
     (org-level-4 ((t (:foreground "#93e0e3"))))                     
     (org-level-5 ((t (:foreground "#7cb8bb"))))                     
     (org-level-6 ((t (:foreground "#6ca0a3"))))                     
     (org-level-7 ((t (:foreground "#5c888b"))))                     
     (org-level-8 ((t (:foreground "#4c7073"))))                     
     (org-link ((t (:foreground "#d0bf8f" :underline t))))           
     ;'(org-priority faces                                            TODO
     (org-scheduled ((t (:foreground "#bfebbf"))))                   
     (org-scheduled-previously ((t (:foreground "#8c5353"))))        
     (org-scheduled-today ((t (:foreground "#94bff3"))))             
     (org-special-keyword ((t (:foreground "#e0cf9f"))))             
     (org-table ((t (:foreground "#9fc59f"))))                       
     (org-tag ((t (:bold t :weight bold))))                          
     (org-time-grid ((t (:foreground "#ffc9a4"))))                   
     (org-upcoming-deadline ((t (:inherit font-lock-keyword-face)))) 
     (org-warning ((t (:bold t :foreground "#cc9393" :weight bold))))

     ;(comint-highlight-input ((t (:italic t :bold t))))
     ;(comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (isearch ((t (:background ,wombat-orange-1 :foreground ,wombat-blue-2))))
     (isearch-lazy-highlight-face ((t (:foreground ,wombat-blue-2 :background ,wombat-orange+1))))

     ;; Parenthesis Matching
     (paren-face-match ((t (:inherit show-paren-match-face))))
     (paren-face-match-light ((t (:inherit show-paren-match-face))))
     (paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
     (show-paren-match-face ((t (:background ,wombat-orange :foreground "white" :bold t))))
     (show-paren-mismatch-face ((t (:background ,wombat-purple-1 :foreground ,wombat-blue-2))))

     (persp-selected-face ((t (:foreground ,wombat-blue-2))))

     (info-xref ((t (:foreground ,wombat-blue))))
     (info-xref-visited ((t (:foreground ,wombat-purple-1))))
     
     ;; Eshell
     (eshell-prompt-face ((t (:foreground "#f0dfaf"))))
     (eshell-ls-archive-face ((t (:foreground "#c3bf9f" :weight bold))))
     (eshell-ls-backup-face ((t (:inherit font-lock-comment))))
     (eshell-ls-clutter-face ((t (:inherit font-lock-comment))))
     (eshell-ls-directory-face ((t (:foreground ,"#94bff3" :bold t :weight bold))))
     (eshell-ls-executable-face ((t (:foreground ,"#dca3a3" :bold t ))))
     (eshell-ls-unreadable-face ((t (:foreground "#606060"))))
     (eshell-ls-missing-face ((t (:inherit font-lock-warning))))
     (eshell-ls-product-face ((t (:inherit font-lock-doc))))
     (eshell-ls-special-face ((t (:foreground "#e0cf9f"))))
     (eshell-ls-symlink-face ((t (:foreground ,"#93e0e3" :bold t :weight bold))))

     ;; Diff mode
     (diff-header ((t (:background "#464646"))))
     (diff-index ((t (:bold t :weight bold))))
     (diff-file-header ((t (:bold t :weight bold))))
     (diff-hunk-header ((t (:background "#464646"))))

     (diff-added ((t (:foreground "#dfdfbf" :bold t :weight bold))))
     (diff-removed ((t (:foreground "#8cd0d3"))))
     (diff-context ((t (:inherit font-lock-comment))))

     )))

(provide 'color-theme-wombat)
