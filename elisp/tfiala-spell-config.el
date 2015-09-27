;; Find the first aspell
(let* ((search-paths `("/usr/bin" ,(concat (getenv "HOME") "/homebrew/bin") "/usr/local/bin"))
       (ispell-path (or
                     (tfiala-first-file-in-path search-paths "aspell")
                     (tfiala-first-file-in-path search-paths "aspell"))))
  (when ispell-path
    (setq-default ispell-program-name ispell-path)))

(provide 'tfiala-spell-config)