;; load marmalade package manager
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ensure required packages are loaded
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      clojure-mode
                      clojure-test-mode
                      magit
                      nrepl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
