(defvar tfiala-keyboard-use-kinesis)
(defvar tfiala-fontsize)
(defvar installed-lisps)

(require 'tfiala-keyboard)

(declare-function color-theme-initialize "color-theme.el" nil)
(declare-function color-theme-dark-blue2 "color-theme.el" nil)

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
  (tfiala-setup-mac-keyboard t))

(defun local-dot-emacs-post ()
  ;; set the color theme
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-solarized-light)
  (print (concat "finishing local-dot-emacs-post for " (system-name))))
