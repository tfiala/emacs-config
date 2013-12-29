(require 'color-theme)
(defvar tfiala-keyboard-use-kinesis)
(defvar installed-lisps)

(defun local-dot-emacs-pre ()
  (print (concat "starting local-dot-emacs-pre for " (system-name)))
  (setq tfiala-keyboard-use-kinesis nil)
  (setq installed-lisps
        '((acl-smp-a ("/lisps/acl90-smp.64/alisp"))
          (acl-smp-m ("/lisps/acl90-smp.64/mlisp"))
          (acl-a ("/lisps/acl90.64/alisp"))
          (acl-m ("/lisps/acl90.64/mlisp"))
          (clisp ("/usr/local/bin/clisp"))
          (sbcl ("/usr/local/bin/sbcl")))))

(defun local-dot-emacs-post ()
  ;; set the color theme
  (require 'color-theme)
  (color-theme-initialize)
  
  ; (color-theme-blue-sea)
  ; (color-theme-robin-hood)
  ; (color-theme-subtle-blue)
  (color-theme-gnome2)
  ; (color-theme-blue-mood)
  ; (color-theme-clarity)
  (print (concat "finishing local-dot-emacs-post for " (system-name))))
