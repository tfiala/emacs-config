;; add current directory to load path
(let ((script-dir (file-name-directory (file-truename load-file-name))))
  (add-to-list 'load-path script-dir))

(require 'tfiala-bootstrap)

(tfiala-per-machine-pre)

(require 'tfiala-package)
(let ((packages
       '(starter-kit
         starter-kit-lisp
         starter-kit-bindings
         starter-kit-eshell
         clojure-mode
         clojure-test-mode
         color-theme
         magit
         nrepl
         org
         slime
         )))
  (tfiala-load-package-list packages))

;; minimal keyboard setup
(when (eq system-type 'darwin)
  (setq mac-control-modifier 'ctrl)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; set default font
(when (not (null window-system))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14")))

;; disable the ugly visible bell
(setq visible-bell nil)

;; org setup
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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

;; set ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; emacs-vc methods
(defun trf-vc-filename (vc-relative-filename)
  (concat (getenv "HOME") "/emacs-vc/" vc-relative-filename))

(defun trf-vc-file-exists-p (vc-relative-filename)
  (file-exists-p (trf-vc-filename vc-relative-filename)))

(require 'slime-config)

(tfiala-per-machine-post)
