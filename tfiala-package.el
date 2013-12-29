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
      (package-install p))))

(provide 'tfiala-package)
