(defvar tfiala-keyboard-use-kinesis nil)

(provide 'tfiala-keyboard)

(defun tfiala-setup-mac-keyboard (is-laptop-keyboard)
  "Setup mac keyboard layout for Ctrl/Meta keys. Optionally uses a latop keyboard setup where right option is mapepd to Ctrl"

  ;; Only apply these settings when this is in a window system
  ;; (i.e. not a terminal).  In terminal mode, the term controls
  ;; the keyboard.
  (when window-system
    (setq mac-control-modifier 'ctrl)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)

    (when is-laptop-keyboard
      (setq mac-right-option-modifier 'ctrl)
      (setq mac-right-command-modifier 'meta)
      ;; Fallback for when we really do have a right control key.
      ;; This would happen if we had a full keyboard plugged in.
      (setq mac-right-control-modifier 'ctrl)
      )
    ))

(defun tfiala-setup-kinesis-keyboard
    (when (eq system-type 'gnu/linux)
      (setq x-super-keysym 'meta)))
