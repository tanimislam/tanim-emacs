;; .emacs

;; add in the packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(package-refresh-contents)
(package-initialize) ;; takes too long

;; now add these packages if not found
(setq pfl-packages
      '(
	ess
	systemd
	magit
	auctex
	markdown-mode
	dracula-theme
	tramp
	jedi
	matlab-mode
	web-mode
	exec-path-from-shell
	sphinx-doc
	sphinx-mode
	))
;; refresh package list if it is not already available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install packages from the list that are not yet installed
(dolist (pkg pfl-packages)
  (when (and (not (package-installed-p pkg))
	     (assoc pkg package-archive-contents))
    (package-install pkg)))

;; from https://stackoverflow.com/a/26776276
;; mapping mac os X key commands
;; home -> beginning-of-line
;; end  -> end-of-line
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;; no start up message
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; removes *messages* from the buffer
(setq-default message-log-max nil)


;; set default font
;; (set-frame-font "Liberation Mono 11")
(setq default-frame-alist
      '((font . "Ubuntu Mono 11")
	(height . 45 )
	(width . 80 ) ) )
(setq initial-frame-alist
      '((font . "Ubuntu Mono 11")
	(height . 45 )
	(width . 80 ) ) )
(add-to-list 'initial-frame-alist '(font . "Ubuntu Mono 11"))

;; add elisp to path
(add-to-list 'load-path "~/.xemacs")
(add-to-list 'load-path "~/.xemacs/elisp")

;; column-number-mode
(setq column-number-mode t)

;; turn on font-lock mode
(global-font-lock-mode)

;; no temporary files, following http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(setq make-backup-files nil)

;; add .bash_aliases to mode
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode ) )

;; (require 'wpdl-mode)
;; mercury mode
(require 'mercury-mode)
(add-to-list 'auto-mode-alist '("\\.inp\\'" . mercury-mode ) )

;; R-modes
(require 'ess-mode)

;; fucking pop-win
(require 'popwin)
(popwin-mode -1)

;; enable visual feedback on selections
(setq transient-mark-mode t)
(setq backup-inhibited t)

;; now auto-load the following
(autoload 'awk-mode "cc-mode" nil t)

;; autoload matlab-mode
(require 'matlab)
(autoload 'matlab-mode "matlab" "Enter matlab mode." t)
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode ) )
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; systemd-mode
(require 'systemd)
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; now load up css-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode ) )
(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode ) )
(setq cssm-indent-function 'cssm-c-style-indenter)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[cq]ss\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for web-mode"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2) )
(add-hook 'web-mode-hook 'my-web-mode-hook )

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (visual-line-mode)))


;; bash_aliases should be in sh-mode
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))

;; swig-mode
(require 'swig-mode)
(add-to-list 'auto-mode-alist '("\\.sw\\'" . swig-mode))

;; systemd-mode
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; now setting up display to the following
(mouse-wheel-mode 1)

;; make LaTeX act more like what I was used to in KILE
(require 'tex )
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (setq flyspell-mode 1)
	    (setq LaTeX-item-indent 0) ;; indent item by 2
	    (visual-line-mode)
	    (LaTeX-math-mode)
	    (column-number-mode)
	    ;; now define some keybindings I grew used to when
	    ;; using kile
	    (define-key LaTeX-mode-map (kbd "C-S-<f6>")
	      (lambda ()
    	(interactive)
	    	(TeX-command-menu "LaTeX")))
	    (define-key LaTeX-mode-map (kbd "C-S-<f5>")
	      (lambda ()
	    	(interactive)
	    	(TeX-command-menu "BibTeX")))
	    (define-key LaTeX-mode-map (kbd "C-c C-p")
	      (lambda ()
		(interactive)
		(TeX-command-menu "View")))
	    (define-key LaTeX-mode-map (kbd "C-^")
	      (lambda ()
	    	(interactive)
	     	(TeX-command-menu "Index")))))

;; (with-eval-after-load "latex"
;;   '(progn
;;      (define-key LaTeX-mode-map (kbd "C-S-<f6>")
;;        (lambda ()
;; 	 (interactive)
;; 	 (TeX-command-menu "LaTeX")))
;;      (define-key LaTeX-mode-map (kbd "C-S-<f5>")
;;        (lambda ()
;; 	 (interactive)
;; 	 (TeX-command-menu "BibTeX")))
;;      (define-key LaTeX-mode-map (kbd "C-^")
;;        (lambda ()
;; 	 (interactive)
;; 	 (TeX-command-menu "Index")))))

(eval-after-load "tex"
  '(progn
     (TeX-add-style-hook
      "cleveref"
      (lambda( )
	(font-latex-add-keywords
	 '(( "cref" "*{" )
	   ( "Cref" "*{" ))
	 'reference))
      :latex )))

(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(put 'upcase-region 'disabled nil)

;; SPELLING
;; brew install apsell
(setq ispell-program-name "/usr/bin/aspell")


;; dracula
(load-theme 'dracula t)

;; ares-mode
(require 'ares-mode)
(add-to-list 'auto-mode-alist '("\\.ares$" . ares-mode))

;; python jedi setup

;; Python Hook
(setq python-indent-guess-indent-offset nil)
(add-hook 'python-mode-hook
          (lambda ( )
	    (visual-line-mode t)
	    (require 'sphinx-doc)
	    (sphinx-doc-mode t)
	    (visual-line-mode)
	    (setq python-indent-offset 0)
	    (setq indent-tabs-mode nil)
	    (setq tab-width 2)))
;; (add-hook 'python-mode-hook 'jedi:setup )
(put 'downcase-region 'disabled nil)

;; Markdown hook
(add-hook 'markdown-mode-hook
	  (lambda ( )
	    (visual-linemode)))

;; Restructed Text Mode Hooks
(add-hook 'rst-mode-hook
	  (lambda ( )
	    (require 'sphinx-mode)
	    (visual-line-mode)))

;; inherit in the $PATH from the shell
(exec-path-from-shell-initialize)
