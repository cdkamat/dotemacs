;; Configuration for org-mode
;; Auto starting org-mode for following file types
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; Settings
(setq org-hide-leading-stars t
      org-directory "~/Documents/Notes"
      org-default-notes-file (concat org-directory "/remember.org")
      org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
      org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps t
      org-use-fast-todo-selection t
      org-completion-use-ido t)

;; Standard org-mode key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t") 'org-todo)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda-list)

(setq org-capture-templates `(("a" "Assignment" entry (file "~/Documents/Notes/assignments.org") "* TODO %^{Assignment desc} %^g%?") 
			      ("c" "Call" entry (file+headline "~/Documents/Notes/remember.org" "Call") "* %? :@Call: %i")
			      ("m" "Meeting" entry (file+headline "~/Documents/Notes/remember.org" "Call") "* %^{Meeting For} :@Call: %^{Time}t %^{Location}p  %i")
			      ("e" "Email" entry (file+headline "~/Documents/Notes/remember.org" "Email") "* %? :@Email: %i")
			      ("n" "Note" entry (file+headline "~/Documents/Notes/Notes.org" "Notes") "* %?  %i") 
			      ("b" "Blog" entry (file+headline "~/Documents/Notes/Notes.org" "Blog") "* %?  %i")))

;; org-todo settings
;; I need more todo keywords than present by default
;; keys mentioned in brackets are hot-keys for the States
;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering the state
(setq org-todo-keywords
      (quote ((sequence "TODO(t/!)" "WORKING(w/!)" "COMPLETED(c!/!)" "REDO(r@/!)" "|" "DONE(d!@/)")
              (sequence "FINDOUT(F)" "WAITING(a@/@)" "SOMEDAY(s)" "|" "CANCELLED(C@/!)"))))
(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("WORKING" :foreground "orange" :weight bold)
 ("REDO" :foreground "magenta" :weight bold)
 ("DONE" :foreground "lightgreen" :weight bold)
 ("FINDOUT" :foreground "red" :weight bold)
 ("WAITING" :foreground "gray" :weight bold)
 ("COMPLETED" :foreground "#ad7fa8" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "lightgreen" :weight bold))))

;; State changes trigger change in tags
(setq org-todo-state-tags-triggers
      (quote (("SOMEDAY" ("WAITING" . t) ("NEXT"))
              (done ("NEXT") ("WAITING") ("SUBMIT"))
	      ("WAITING" ("NEXT") ("SUBMIT") ("WAITING" . t))
              ("TODO" ("WAITING"))
              ("WORKING" ("WAITING") ("SUBMIT") ("NEXT" . t))
	      ("COMPLETED" ("NEXT") ("SUBMIT" . t)))))

;; Tag list
(setq org-tag-alist (quote ((:startgroup)
                            ("@Home" . ?h)
			    ("@Self" . ?s)
			    ("@Email". ?e)
			    ("@Call" . ?C)
			    (:endgroup)
			    ("NEXT" . ?N)
			    ("NOTE" . ?n)
			    ("WAITING" . ?a)
			    ("BLOG" . ?b)
			    (:startgroup)
			    ("CRITICAL" . ?c)
			    ("Minor" . ?m)
			    (:endgroup))))


;; Custom agenda views
(setq org-agenda-custom-commands
      (quote (("s" "Started Tasks" todo "WORKING" ((org-agenda-todo-ignore-with-date nil)))
              ("q" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
	      ("C" "Calls" tags-todo "+@Call" ((org-use-tag-inheritance t)))
	      ("e" "Mails" tags-todo "+@Email" ((org-agenda-todo-ignore-with-date t)))
	      ("h" "Home" tags-todo "+@Home" ((org-agenda-todo-ignore-with-date t))))))

;; Agenda settings
(add-hook 'org-agenda-mode-hook '(lambda ()
				   (hl-line-mode 1)))
(setq org-agenda-repeating-timestamp-show-all t
      org-agenda-show-all-dates t
      org-agenda-start-on-weekday nil
      org-deadline-warning-days 10
      org-agenda-todo-ignore-with-date nil
      org-agenda-skip-deadline-if-done t
      org-agenda-include-inactive-timestamps nil
      org-agenda-skip-scheduled-if-done nil
      org-agenda-text-search-extra-files (quote (agenda-archives))
      org-enforce-todo-dependencies t)

(setq org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down category-up)
              (todo todo-state-up priority-up)
              (tags priority-down))))

;; setup for Reminder
;; Erase all reminders and rebuilt reminders for today from the agenda
(defadvice org-agenda-to-appt (before wickedcool activate)
  "Clear the appt-time-msg-list."
  (setq appt-time-msg-list nil))

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
(appt-activate t)
;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'org-agenda-to-appt)

(setq appt-message-warning-time '15)
(setq appt-display-interval '10)
(setq org-export-with-archived-trees t)
(setq org-export-with-sub-superscripts nil)

; Update appt each time agenda opened.
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; YAS support

(defun yas/org-very-safe-expand ()
                 (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
	    (make-variable-buffer-local 'yas/trigger-key)
	    (setq yas/trigger-key [tab])
	    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	    (define-key yas/keymap [tab] 'yas/next-field)
	    ;; flyspell mode to spell check everywhere
	    (flyspell-mode 1)))

;; Export org table as CSV by default
(setq org-table-export-default-format "orgtbl-to-csv")

;; Org Latex export
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
      '("org-article"
         "\\documentclass{article}
          \\usepackage{verbatim}
          \\usepackage{color}
          \\usepackage[left=1in,top=1in,right=1in,bottom=1in,head=0.2in,foot=0.2in]{geometry}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-export-latex-todo-keyword-markup '(("NEW" . "\\new{%s}") ("DONE" . "\\done{%s}") ("TODO" . "\\todo{%s}") ("REDO" . "\\redo{%s}") ("CANCELLED" . "\\cancelled{%s}")))

(provide 'org-mode-config)
