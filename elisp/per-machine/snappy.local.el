(defvar tfiala-keyboard-use-kinesis)
(defvar tfiala-fontsize)
(defvar installed-lisps)

(declare-function color-theme-initialize "color-theme.el" nil)
(declare-function color-theme-dark-blue2 "color-theme.el" nil)

(defun local-dot-emacs-pre ()
  (print (concat "starting local-dot-emacs-pre for " (system-name)))
  (setq tfiala-keyboard-use-kinesis nil)
  (setq tfiala-fontsize 16)
  (setq installed-lisps
        '((acl-smp-a ("/lisps/acl90-smp.64/alisp"))
          (acl-smp-m ("/lisps/acl90-smp.64/mlisp"))
          (acl-a ("/lisps/acl90.64/alisp"))
          (acl-m ("/lisps/acl90.64/mlisp"))
          (clisp ("/usr/local/bin/clisp"))
          (cmucl ("/usr/local/bin/lisp"))
          (sbcl ("/usr/local/bin/sbcl")))))

(defun local-dot-emacs-post ()
  ;; set the color theme
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-dark-blue2)
  (print (concat "finishing local-dot-emacs-post for " (system-name))))
