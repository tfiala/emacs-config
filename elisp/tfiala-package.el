(defvar package-archives)
(defvar package-archive-contents)
(declare-function package-installed-p "package.el" (package))

(defun tfiala-load-unbundled-package ()
  "Load the external package.el if it can be found; return non-nil on success, nil otherwise"
  (let ((package-dir (concat (getenv "HOME") "/emacs-vc/package")))
    (when (file-exists-p (concat package-dir "/package.el"))
      (add-to-list 'load-path package-dir))))

(defun tfiala-require-package-system ()
  "Return non-nil if package is available; else, returns nil"
  (or (>= emacs-major-version 24)
      (tfiala-load-unbundled-package)))

(if (tfiala-require-package-system)
    ;; we have the package system one way or another - so define the real operations
    (progn
      (require 'package)

      (defun tfiala-load-package-list
	(package-list)

	(require 'package)
	(add-to-list 'package-archives
		     '("melpa" . "http://melpa.milkbox.net/packages/"))
	(add-to-list 'package-archives
		     '("org" . "http://orgmode.org/elpa/"))
	
	;; load these now, not after the init/customization loop
	;; (see http://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html)
	(package-initialize)
	(setq package-enable-at-startup nil)

	(when (not package-archive-contents)
	  (package-refresh-contents))

	(dolist (p package-list)
	  (when (not (package-installed-p p))
	    (package-install p)))))

  ;; we don't have the package system - package loading becomes a no-op
  (defun tfiala-load-package-list (package-list) ))

(provide 'tfiala-package)
