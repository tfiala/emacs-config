(require 'cl-lib)

(add-hook 'after-make-frame-functions
          (lambda (frame)
	    (let ((solarized-color (or (getenv "SOLARIZED") "light")))
	      (message (concat "setting solarized: " solarized-color " (getenv \"SOLARIZED\") = " (getenv "SOLARIZED")))
	      (set-frame-parameter frame 'background-mode (intern solarized-color))
	      (enable-theme 'solarized))))

(defun tfiala-toggle-background-mode ()
  "Toggle the background mode between light and dark."
  (interactive
   (cl-flet ((new-color-from-old (old-color)
                              (cond ((eq old-color 'light) 'dark)
                                    (t 'light))))
     (let ((new-color (new-color-from-old (or (frame-parameter nil 'background-mode) 'light))))
       (message (concat "setting background-mode: " (symbol-name new-color)))
       (set-frame-parameter nil 'background-mode new-color)
       (enable-theme 'solarized)))))

(provide 'tfiala-display-helpers)
