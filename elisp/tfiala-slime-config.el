;; Slime integration for Lisp
(require 'cl)
(require 'cl-lib)

;; slime forward declarations
;; (we don't want to load slime at this point)
(defvar inferior-lisp-program)
(defvar slime-complete-symbol*-fancy)
(defvar slime-complete-symbol-function)
(defvar slime-default-lisp)
(defvar slime-lisp-implementations)

(defun tfiala-slime-config/get-lisp-exe (lisp-desc)
  (cl-caadr lisp-desc))

(defun tfiala-slime-config/lisp-exists-p (lisp-desc)
  "takes a multi-slime config entry and returns non-nil if the exe exists"
  (file-exists-p (tfiala-slime-config/get-lisp-exe lisp-desc)))

;; if installed lisps are specified and at least one exists,
;; we'll cofigure slime mode.
(when (boundp 'installed-lisps)
  (let ((available-lisps (remove-if-not #'tfiala-slime-config/lisp-exists-p installed-lisps)))
    (when (not (null available-lisps))
      ;; found a lisp, we can setup
      (require 'slime-autoloads)

      ;; setup slime multiple lisp support
      (setq slime-lisp-implementations available-lisps)
      (setq slime-default-lisp (caar available-lisps))

      ;; set the default lisp to the first lisp in the list
      (setq inferior-lisp-program (tfiala-slime-config/get-lisp-exe (car available-lisps)))

      (eval-after-load "slime"
        '(progn
           (slime-setup '(slime-fancy slime-banner slime-editing-commands))
           (setq slime-complete-symbol*-fancy t)
           (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

           ;; bind some global keys
           (global-set-key (kbd "C-c s") 'slime-selector))))))

(provide 'tfiala-slime-config)
