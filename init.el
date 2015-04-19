(require 'cl-lib)

;; add our elisp dir
(let ((elisp-dir (concat (file-name-directory load-file-name) "elisp")))
  (add-to-list 'load-path elisp-dir))

(require 'tfiala-bootstrap)
(require 'tfiala-package)
(require 'tfiala-keyboard)

;;
;; load desired packages
;;
;; Note: this becomes a do nothing if emacs version
;; is less than 24 and there is no local package.el
;; found in this directory: $HOME/emacs-vc
;;
(let ((always-load-packages '(cider
                              ac-cider
                              clojure-mode
			      color-theme
                              color-theme-solarized
                              company
                              company-ghc
                              exec-path-from-shell
                              flycheck
                              flycheck-haskell
                              ghc
                              haskell-mode
                              hi2
			      magit
			      org
                              rainbow-delimiters
                              rainbow-mode
                              slime
                              tuareg))
      (additional-packages
       (cond
	((>= emacs-major-version 24) '(starter-kit
				       starter-kit-lisp
				       starter-kit-eshell))
	(t '()))))

  (tfiala-load-package-list (append additional-packages always-load-packages)))

;; Before we do anything else, ensure our path is set correctly.  For
;; now just do this on MacOSX.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "EDITOR"))

(tfiala-per-machine-pre)

;; minimal keyboard setup
(when (and (boundp 'tfiala-keyboard-use-kinesis)
           tfiala-keyboard-use-kinesis)
  (tfiala-setup-kinesis-keyboard))

;; set default font
(when window-system
  (let* ((font-size (or (and (boundp 'tfiala-fontsize)
                            tfiala-fontsize)
                        12))
         (font-name (format "Source Code Pro-%d" font-size)))
    (print (concat "using font " font-name))
    (add-to-list 'default-frame-alist `(font . ,font-name))))

;;
;; visual tweaks
;;
;; Some of these settings are redundant if the starter-kit packages
;; are available and loaded.  This covers us in the case that the
;; starter kit packages are not available.
;;

(transient-mark-mode 1)

;; move scroll bars to right
(when window-system
  (set-scroll-bar-mode 'right))

;; disable splashscreen
(setq inhibit-splash-screen t)

;; ido mode
(require 'ido)
(ido-mode t)

;; always use y or n for yes-or-no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; compiler output tracking
(setq compilation-scroll-output t)

;; disable the ugly visible bell
(setq visible-bell nil)

;; set ispell
(require 'tfiala-spell-config)

(require 'tfiala-org-config)

;; start the server if it's not running
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; whitespace handling
(require 'whitespace)
(global-whitespace-mode)

;; gdb-mode fix for long load time on Emacs 24.
(setq gdb-create-source-file-list nil)

;; OCaml support
;; FIXME move to 'tfiala-ocaml-config
(defun setup-ocaml-mode ()
  (require 'tuareg)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode))
                auto-mode-alist))
  ;; Tweak for problem on OS X where Emacs.app doesn't run the right
  ;; init scripts when invoking a sub-shell
  (cond
   ((eq window-system 'ns) ; macosx
    ;; Invoke login shells, so that .profile or .bash_profile is read
    (setq shell-command-switch "-lc")))
  ;; Setup environment variables using opam
  (defun opam-vars ()
    (let* ((x (shell-command-to-string "opam config env"))
           (x (split-string x "\n"))
           (x (cl-remove-if (lambda (x) (equal x "")) x))
           (x (mapcar (lambda (x) (split-string x ";")) x))
           (x (mapcar (lambda (x) (car x)) x))
           (x (mapcar (lambda (x) (split-string x "=")) x))
           )
      x))
  (dolist (var (opam-vars))
    (setenv (car var) (substring (cadr var) 1 -1)))
  (setq exec-path (split-string (getenv "PATH") path-separator))
  ;; Update the emacs load path
  (push (concat (getenv "OCAML_TOPLEVEL_PATH")
                "/../../share/emacs/site-lisp") load-path)
  ;; Automatically load utop.el
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

(require 'tfiala-clojure-config)
(require 'tfiala-slime-config)

(require 'tfiala-haskell-config)

;; python setup
(add-hook 'python-mode-hook 'flycheck-mode)

;; Enable company-mode everywhere (complete-any mode).
(global-company-mode)

;; Accounting related.
(require 'tfiala-beancount-config)

(require 'tfiala-display-helpers)

;;
;; KEEP THIS AT THE BOTTOM OF THE init.el FILE.
;;
;; This is the hook whereby per-machine configuration can adjust any
;; of the defaults before emacs initialization completes.
;;
(tfiala-per-machine-post)
