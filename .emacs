;; load marmalade package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; ensure required packages are loaded
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      clojure-mode
                      clojure-test-mode
                      ;; cperl-mode
                      magit
                      nrepl
                      org))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; cperl setup
;; (defalias 'perl-mode 'cperl-mode)
;; (setq cperl-hairy t)

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

;; set ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; emacs-vc methods
(defun trf-vc-filename (vc-relative-filename)
  (concat (getenv "HOME") "/emacs-vc/" vc-relative-filename))

(defun trf-vc-file-exists-p (vc-relative-filename)
  (file-exists-p (trf-vc-filename vc-relative-filename)))

;; setup go mode
(when (trf-vc-file-exists-p "go-mode/go-mode-load.el")
  (add-to-list 'load-path (trf-vc-filename "go-mode"))
  ;; (require 'go-mode-load)
  (require 'go-mode)
  )
