;; Slime integration for Lisp

(defun slime-config/get-lisp-exe (lisp-desc)
  (caadr lisp-desc))

(defun slime-config/lisp-exists-p (lisp-desc)
  "takes a multi-slime config entry and returns non-nil if the exe exists"
  (file-exists-p (slime-config/get-lisp-exe lisp-desc)))

;; if installed lisps are specified and at least one exists,
;; we'll cofigure slime mode.
(when (boundp 'installed-lisps)
  (let ((available-lisps (remove-if-not #'slime-config/lisp-exists-p installed-lisps)))
    (when (not (null available-lisps))
      ;; set the inferior lisp to the first (i.e. preferred) lisp in the list
      (setq inferior-lisp-program (slime-config/get-lisp-exe (car available-lisps)))

      ;; found a lisp, we can setup 
      (require 'slime-autoloads)

      (setq slime-config/available-lisps available-lisps)
      ;; setup the slime vars that support multiple lisps
      (setq slime-lisp-implementations slime-config/available-lisps)
      (setq slime-default-lisp (caar slime-config/available-lisps))
      
      (eval-after-load "slime"
        '(progn
           (slime-setup '(slime-fancy slime-banner slime-editing-commands))
           (setq slime-complete-symbol*-fancy t)
           (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

           ;; bind some global keys
           (global-set-key (kbd "C-c s") 'slime-selector))))))

(provide 'slime-config)
