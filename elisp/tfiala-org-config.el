(require 'org)

;; key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; org agenda files - use google drive's org dir
(unless (boundp 'org-agenda-files)
  (setq org-agenda-files '()))
(let ((org-dir (concat (getenv "HOME") "/.org.d")))
  (when (file-exists-p org-dir)
    (add-to-list 'org-agenda-files org-dir)))

(setq org-catch-invisible-edits 'show-and-error)
(setq org-startup-indented t)

;; Specify additional TODO workflow states, along with whether time
;; stamps and notes are specified with them.
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "XFER(x@/!)")))

;; When tracking state changes and notes, put them in a drawer with
;; this name.
(setq org-log-into-drawer "LOGBOOK")

;; Enforce TODO depedencies - don't allow marking a parent as DONE
;; when any of the children are still not DONE.
(setq org-enforce-todo-dependencies t)

(provide 'tfiala-org-config)
