(require 'cl-lib)

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
       '(company
	 doom-themes
	 exec-path-from-shell
	 helm
	 helm-projectile
	 magit
	 markdown-mode
	 ;; org
	 org-journal
	 paredit
	 projectile
	 rainbow-delimiters
	 slime
	 use-package
	 )))
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(eval-when-compile
  (require 'use-package))

;;
;; Get exec-path setup right
;;

(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "EDITOR"))

;;
;; Backup file handling
;;
(setq backup-directory-alist `(("." . "~/.saves")))

;;
;; Keyboard setup
;;

(when (eq window-system 'ns)
  (setq mac-command-modifier 'meta))

;;
;; yes-or-no questions should accept just 'y' or 'n'
;;

(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; Setup fonts
;;

(when window-system
  (let* ((font-size
	  (or (and (boundp 'tfiala-fontsize) tfiala-fontsize) 14))
	 (font-name (format "MesloLGM Nerd Font-%d" font-size)))
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
;; Setup Lisp
;;

(require 'subr-x)

(defun tfiala-slime-lisp-for-maybe (name lisp-binary &optional core-arg core-file)
  (when-let ((lisp-binary-full-path (executable-find lisp-binary))
	     (name-sym (intern name)))
    (if-let* ((core-file-path (and core-file (expand-file-name core-file)))
	      (_ (file-readable-p core-file-path)))
	`(,name-sym (,lisp-binary-full-path ,core-arg ,core-file-path))
      `(,name-sym (,lisp-binary-full-path)))))

(setq tfiala-lisps
      `(("clozure" "ccl64" "--image-name" "~/bin/ccl.core-for-slime")
	("sbcl" "sbcl" "--core" "~/bin/sbcl.core-for-slime")
	("lw8" "~/bin/lw-8.0-console")
	("lw7" "~/bin/lw-7.1-console"))
      )

(setq slime-lisp-implementations
      (seq-remove #'null
		  (mapcar (lambda (lisp-desc)
			    (apply #'tfiala-slime-lisp-for-maybe lisp-desc))
			  tfiala-lisps)))

(setq slime-contribs '(slime-fancy slime-banner))
(require 'slime-autoloads)

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
;; Org setup
;;

;; FIXME autoload this
(require 'org)

;; Enable graphviz and dot in org-mode code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (dot . t)
   (emacs-lisp . t)
   (shell . t)))

;; Support clojure in org-mode
(add-hook
 'org-mode-hook
 (lambda ()
   (require 'ob-clojure)
   (setq org-babel-clojure-backend 'cider)
   (require 'cider)
   (setq org-edit-src-content-indentation 0
	 org-src-tab-acts-natively t
	 org-src-fontify-natively t
	 org-confirm-babel-evaluate nil
	 org-support-shift-select 'always
	 )))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file "~/organizer.org")))

(setq org-directory (concat (getenv "HOME") "/Dropbox/org/"))
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-log-done 'time)
(setq org-todo-keywords '((sequence "TODO" "WAIT" "|" "DONE" "DELEGATED")))

;; org: visuals
(setq org-startup-indented t)

;; org: agenda
(setq org-agenda-files (list (concat org-directory "taskdiary.org")))
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|[0-9]+\\'")
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline
				  "taskdiary.org" "Calendar")
	 "* APPT %^{Description} %^g
%?
Added: %U")
	("n" "Notes" entry (file+datetree
			    "taskdiary.org")
	 "* %^{Description} %^g %?
Added: %U")
	("t" "Task Diary" entry (file+datetree
				 "taskdiary.org")
	 "* TODO %^{Description}  %^g
%?
Added: %U")
	("l" "Log Time" entry (file+datetree
			       "timelog.org" )
	 "** %U - %^{Activity}  :TIME:")
	))

;; org-journal
(setq org-journal-dir (concat org-directory "journal/"))
(add-to-list 'org-agenda-files org-journal-dir)
;; Prevent carrying TODO items over to the next day.  I find this is
;; losing context with the meeting where I took the notes.  Agenda
;; view already covers these.
(setq org-journal-carryover-items nil)
(require 'org-journal)

;;
;; C/C++ styles
;;


;; Add a cc-mode style for editing LLVM C and C++ code
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
	    (+ (aref in-assign 0)
	       (* 2 c-basic-offset)))
      in-assign)))

(c-add-style "llvm.org"
	     '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))

(c-add-style "tfiala"
	     '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 4)
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))

(setq c-default-style "tfiala")

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
(add-hook 'c-mode-common-hook
	  (function
	   (lambda nil 
	     (if (string-match "llvm" buffer-file-name)
		 (progn
		   (c-set-style "llvm.org"))))))

    ;; flyspell setup
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)

    ;; markdown-mode
    (setq markdown-command "/usr/local/bin/pandoc")

    (put 'narrow-to-region 'disabled nil)

    ;; specify lisp program
    ;; (setq inferior-lisp-program "/Users/tfiala/bin/lw-7.1-console")
    (setq inferior-lisp-program "/Users/tfiala/bin/lw-8.0-console")

;;
;; color themes
;;
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package slime rainbow-delimiters paredit org-journal markdown-mode magit helm-projectile exec-path-from-shell doom-themes company cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
