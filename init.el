;; add our elisp dir
(let ((elisp-dir (concat (file-name-directory load-file-name) "elisp")))
  (add-to-list 'load-path elisp-dir))

(require 'tfiala-package)

(let ((packages
       '(starter-kit
         starter-kit-lisp
         starter-kit-bindings
         starter-kit-eshell
         clojure-mode
         clojure-test-mode
	 cider
         color-theme
         magit
         org
         slime
         )))
  (tfiala-load-package-list packages))

(require 'tfiala-bootstrap)
(tfiala-per-machine-pre)

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

(require 'tfiala-org-config)

;; set ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; emacs-vc methods
(defun trf-vc-filename (vc-relative-filename)
  (concat (getenv "HOME") "/emacs-vc/" vc-relative-filename))

(defun trf-vc-file-exists-p (vc-relative-filename)
  (file-exists-p (trf-vc-filename vc-relative-filename)))

(require 'tfiala-slime-config)
;;(load "slime-config")

(tfiala-per-machine-post)
