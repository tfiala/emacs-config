;; Check if we have the beancount elisp source.  If so, load it.
(let ((beancount-elisp-dir (concat (getenv "HOME") "/src/finances/beancount/src/elisp")))
  (when (file-exists-p beancount-elisp-dir)
    (add-to-list 'load-path beancount-elisp-dir)
    (require 'beancount)
    (add-to-list 'auto-mode-alist '("\\.beancount$" . beancount-mode))))

(provide 'tfiala-beancount-config)
