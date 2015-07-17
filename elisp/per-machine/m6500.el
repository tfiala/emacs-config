(defvar tfiala-keyboard-use-kinesis)
(defvar tfiala-fontsize)
(defvar installed-lisps)

(require 'tfiala-keyboard)

(declare-function color-theme-initialize "color-theme.el" nil)

(defun local-dot-emacs-pre ()
  ;; Set up variables that will control session behavior.
  (print (concat "starting local-dot-emacs-pre for " (system-name)))
  (setq tfiala-keyboard-use-kinesis nil)
  (setq tfiala-fontsize 16)
  (setq installed-lisps
        '((acl-smp-a ("/lisps/acl90-smp.64/alisp"))
          (acl-smp-m ("/lisps/acl90-smp.64/mlisp"))
          (acl-a ("/lisps/acl90.64/alisp"))
          (acl-m ("/lisps/acl90.64/mlisp"))
          (ccl ("/usr/local/bin/ccl64"))
          (clisp ("/usr/local/bin/clisp"))
          (cmucl ("/usr/local/bin/lisp"))
          (sbcl ("/usr/local/bin/sbcl"))))

  ;; Setup keyboard handling.
  )

(defun local-dot-emacs-post ()
  ;; set the color theme
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-initialize)
  (when (not window-system)
    (let ((solarized-color-string (or (getenv "SOLARIZED") "light")))
      (setq frame-background-mode (intern solarized-color-string))
      (set-terminal-parameter nil 'background-mode (intern solarized-color-string))))
  (load-theme 'solarized t)
  (print (concat "finishing local-dot-emacs-post for " (system-name))))
