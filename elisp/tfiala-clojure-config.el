;; Turn on rainbow-delimiters for clojure and cider files.
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Ensure cider eldoc integration is enabled.
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide some of the working cider buffers like *nrepl-connection* and
;; *nrepl-server*.
(setq nrepl-hide-special-buffers t)

;; Make C-c C-z switch to the CIDER REPL buffer in the current window.
;; (setq cider-repl-display-in-current-window t)

;; To adjust the maximum number of items kept in the REPL history.
(setq cider-repl-history-size 1000) ; the default is 500

;; To store the REPL history in a file.
(setq cider-repl-history-file (concat (getenv "HOME") "/.emacs.d/.cider-history"))

;; Enable paredit mode in the cider repl.
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Change the result prefix for REPL evaluation (by default there's no prefix).
(setq cider-repl-result-prefix ";;= ")

;; Change the result prefix for interactive evaluation (by default it's =>).
(setq cider-interactive-eval-result-prefix ";;= ")

;; Normally code you input in the REPL is font-locked with
;; cider-repl-input-face (after you press RET) and results are
;; font-locked with cider-repl-result-face. If you want them to be
;; font-locked as in clojure-mode use the following:
(setq cider-repl-use-clojure-font-lock t)

(provide 'tfiala-clojure-config)
