(require 'cl-lib)

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

(defun tfiala-directory-plus-element
  (directory final-path-element)
  "Indicate if the directory and the given path element exists, returning it if it does, or nil otherwise."
  (concat (file-name-as-directory directory) (file-name-nondirectory final-path-element)))

(defun tfiala-first-file-in-path
    (directories program-name)
  "Return the first full path to the program-name that exists using each of the given directories."
  (let ((present-paths (cl-remove-if-not 'file-exists-p (mapcar (lambda (x) (tfiala-directory-plus-element x program-name)) directories))))
     (car present-paths)))

;;
;; package helper functions
;;
;; (defun tfiala-get-dir-for-package (package-symbol)
;;   (elt (cadr (assoc package-symbol package-alist)) 7))

(provide 'tfiala-bootstrap)
