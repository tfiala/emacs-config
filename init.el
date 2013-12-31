;; add our elisp dir
(let ((elisp-dir (concat (file-name-directory load-file-name) "elisp")))
  (add-to-list 'load-path elisp-dir))

(require 'tfiala-bootstrap)

;;
;; load desired packages
;;
;; Note: this becomes a do nothing if emacs version
;; is less than 24 and there is no local package.el
;; found in this directory: $HOME/emacs-vc
;;
(require 'tfiala-package)

(let ((always-load-packages '(clojure-mode
			      clojure-test-mode
			      cider
			      color-theme
			      magit
			      org
			      slime))
      (additional-packages
       (cond 
	((>= emacs-major-version 24) '(starter-kit
				       starter-kit-lisp
				       starter-kit-eshell))
	(t '()))))
  
  (tfiala-load-package-list (append additional-packages always-load-packages)))

(tfiala-per-machine-pre)

;; minimal keyboard setup
(when (eq system-type 'darwin)
  (setq mac-control-modifier 'ctrl)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(when tfiala-keyboard-use-kinesis
  (when (eq system-type 'gnu/linux)
    (setq x-super-keysym 'meta)))

;; kinesis keyboard on linux
(setq tfiala-use-kinesis t)
(when tfiala-use-kinesis
  (when (eq system-type 'gnu/linux)
    (setq x-super-keysym 'meta)))

;; set default font
(when (not (null window-system))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12")))

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
(when (file-exists-p "/usr/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))

(when (file-exists-p "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/local/bin/aspell"))

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

(require 'tfiala-slime-config)

;;
;; KEEP THIS AT THE BOTTOM OF THE init.el FILE.
;;
;; This is the hook whereby per-machine configuration can adjust any
;; of the defaults before emacs initialization completes.
;;
(tfiala-per-machine-post)
