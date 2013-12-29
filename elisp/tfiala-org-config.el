(require 'org)

;; key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; org agenda files - use google drive's org dir
(unless (boundp 'org-agenda-files)
  (setq org-agenda-files '()))
(let ((org-dir (concat (getenv "HOME") "/Google Drive/org")))
  (when (file-exists-p org-dir)
    (add-to-list 'org-agenda-files org-dir)))

(setq org-catch-invisible-edits 'show-and-error)
(setq org-startup-indented t)
(setq org-todo-keywords
      '((sequence "WAITING(w)" "TODO(t)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)")))

(provide 'tfiala-org-config)
