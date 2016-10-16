;;
;; Load packages
;;

;; setup package loading
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

;; Ensure we only refresh package contents when we don't already
;; have any (i.e. first install); otherwise, only update on user
;; timeline.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install packages.
(let ((packages
       '(alchemist
	 cider
	 clojure-mode
	 color-theme
	 color-theme-solarized
	 company
	 elixir-mode
	 exec-path-from-shell
	 helm
	 helm-projectile
	 magit
	 org
	 paredit
	 projectile
	 rainbow-delimiters
	 ;; Ruby end-mode is used here for Elixir
	 ruby-end
	 ;; For Elixir, also adapted from Ruby
	 smartparens
	 )))
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;
;; Get exec-path setup right
;;

(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "EDITOR"))

;;
;; yes-or-no questions should accept just 'y' or 'n'
;;

(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; Setup fonts
;;

(when window-system
  (let* ((font-size
	  (or (and (boundp 'tfiala-fontsize) tfiala-fontsize)
	      16))
         (font-name (format "Source Code Pro-%d" font-size)))
    (print (concat "using font " font-name))
    (add-to-list 'default-frame-alist `(font . ,font-name))))

;;
;; Hide welcome screen
;;

(setq inhibit-startup-screen t)

;;
;; No toolbar
;;

(tool-bar-mode -1)

;;
;; No visible bell
;;

(setq visible-bell nil)

;;
;; We want to see trailing whitespace
;;

(require 'whitespace)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c t") 'whitespace-toggle-options)

;;
;; Setup color theme
;;

(defun solarized-is-light-p ()
  (if window-system
      (eq (frame-parameter (selected-frame) 'background-mode) 'light)
    (eq (terminal-parameter nil 'background-mode) 'light)))

(defun solarized-set (use-light)
  (let ((background (if use-light 'light 'dark)))
    (set-frame-parameter nil 'background-mode background)
    (when (not window-system)
      (set-terminal-parameter nil 'background-mode background)))
  (enable-theme 'solarized))

(defun solarized-toggle ()
  (interactive)
  (solarized-set (not (solarized-is-light-p))))

(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)
(load-theme 'solarized t)

(global-set-key (kbd "C-c s") 'solarized-toggle)

;; Initialize solarized to env-var SOLARIZED, which should be
;; "light" or "dark".  When not set, use "light".
(solarized-set (string= (or (getenv "SOLARIZED") "light") "light"))

;;
;; Setup dired
;;

(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "-aBhl --group-directories-first")

;; Use dired-x
(add-hook
 'dired-load-hook
 (lambda ()
   (load "dired-x")
   ;; Set dired-x global variables here.  For example:
   ;; (setq dired-guess-shell-gnutar "gtar")
   ;; (setq dired-x-hands-off-my-keys nil)
   ))
(add-hook
 'dired-mode-hook
 (lambda ()
   ;; Set dired-x buffer-local variables here.
   (dired-omit-mode 1)
   ))

;; Enable dired-jump and dired-jump-other-window before
;; dired is loaded.
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

;;
;; Setup paredit
;;

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(let ((lispen-hooks
       '(emacs-lisp-mode-hook
	 eval-expression-minibuffer-setup-hook
	 lisp-mode-hook
	 lisp-interaction-mode-hook
	 scheme-mode-hook)))
  (seq-do
   #'(lambda (x) (add-hook x #'enable-paredit-mode))
   lispen-hooks))

;;
;; Setup rainbow delimiters (rainbow parens)
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Always show matching paren
;;

(show-paren-mode 1)

;;
;; Setup projectile
;;

(require 'projectile)

;; run projectile everywhere
(projectile-global-mode)

;;
;; Setup helm
;;
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; Integrate helm with projectile
(require 'helm-projectile)
(helm-projectile-on)

;;
;; company autocompletion support
;;

(require 'company)

;; use company everywhere
(global-company-mode)

;;
;; Elixir support
;;

(require 'elixir-mode)
(require 'alchemist)
(require 'smartparens)

;; Elixir adaptation of Ruby end mode
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

;; Elixir adaptation of smartparens
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))
