;; add current directory to load path
(let ((script-dir (file-name-directory (file-truename load-file-name))))
  ;; initialize per-machine setup functions
  (let ((per-machine-file (concat script-dir "per-machine/" (system-name) ".el")))
    (when (file-exists-p per-machine-file)
      (load per-machine-file))))

(defun tfiala-per-machine-pre ()
  (when (fboundp 'local-dot-emacs-pre)
    (local-dot-emacs-pre)))

(defun tfiala-per-machine-post ()
  (when (fboundp 'local-dot-emacs-post)
    (local-dot-emacs-post)))

;;
;; package helper functions
;;
;; (defun tfiala-get-dir-for-package (package-symbol)
;;   (elt (cadr (assoc package-symbol package-alist)) 7))

(provide 'tfiala-bootstrap)
