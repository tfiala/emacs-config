;; load marmalade package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

;; load these now, not after the init/customization loop
;; (see http://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html)
(package-initialize)
(setq package-enable-at-startup nil)

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
                      org
                      slime
                      ))

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

;; setup go mode
(when (trf-vc-file-exists-p "go-mode/go-mode-load.el")
  (add-to-list 'load-path (trf-vc-filename "go-mode"))
  ;; (require 'go-mode-load)
  (require 'go-mode)
  )

;;
;; package helper functions
;;

(defun tfiala-get-dir-for-package (package-symbol)
  (elt (cadr (assoc package-symbol package-alist)) 7))

;;
;; setup slime mode
;;
(setq inferior-lisp-program "/lisps/acl90-smp.64/alisp")
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
     ;; (let ((slime-contrib-dir
     ;;        (concat (tfiala-get-dir-for-package 'slime) "/contrib")))
     ;;   (print slime-contrib-dir)
     ;;   (when (file-exists-p slime-contrib-dir)
     ;;     (add-to-list 'load-path slime-contrib-dir)))

     ;; (require 'slime-fancy)
     ;; (slime-fancy-init)
     ;; (require 'slime-banner)
     ;; (slime-banner-init)
     
     (slime-setup '(slime-fancy slime-banner))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

