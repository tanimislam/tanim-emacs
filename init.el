;; .emacs

;; add in the packages
;; cargo cult from https://melpa.org/#/getting-started
;;(require 'package)
;;(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                    (not (gnutls-available-p))))
;;       (proto (if no-ssl "http" "https")))
;;  (when no-ssl (warn "\
;;Your version of Emacs does not support SSL connections,
;;which is unsafe because it allows man-in-the-middle attacks.
;;There are two things you can do about this warning:
;;1. Install an Emacs version that does support SSL and be safe.
;;2. Remove this warning from your init file so you won't see it again."))
;;  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;  )
;;(package-initialize)

;; cargo cult #2, also from https://melpa.org/#/getting-started
;; also, emacs has REAL problems with https, so use http URLs if at all possible
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(package-refresh-contents)

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

;; now setting up display to the following
(mouse-wheel-mode 1)

;; fucking pop-win
(require 'popwin)
(popwin-mode -1)

;; enable visual feedback on selections
(setq transient-mark-mode t)
(setq backup-inhibited t)

;;;;;; MODE SPECIFIC CHANGES ;;;;;;



;; add .bash_aliases to mode and auto-wrap 
(add-to-list 'auto-mode-alist '("\\.bash_aliases$" . sh-mode ) )
(add-hook 'sh-mode-hook
	  (lambda ( )
	    (visual-line-mode t)))


;; ;; (require 'wpdl-mode)
;; ;; mercury mode
(require 'mercury-mode)
(add-to-list 'auto-mode-alist '("\\.inp$" . mercury-mode ) )
(add-to-list 'auto-mode-alist '("\\.INP$" . mercury-mode ) )
(add-hook 'mercury-mode-hook
	  (lambda ()
	    (visual-line-mode t)))


;; R-modes, not working now, don't know when it will work again
;; (require 'ess-site)

;; systemd-mode
;;(require 'systemd)
;;(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; ;; css-mode
;; (autoload 'css-mode "css-mode")
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode ) )
;; (add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode ) )
;; (setq cssm-indent-function 'cssm-c-style-indenter)

;; ;; web-mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[cq]ss\\'" . web-mode))
;; (add-hook 'web-mode-hook
;; 	  (lambda ( )
;; 	    "Hooks for web-mode"
;; 	    (setq web-mode-markup-indent-offset 2)
;; 	    (setq web-mode-css-indent-offset 2) ) )

;; emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook
 	  (lambda ()
 	    (visual-line-mode)))
	    
;; make LaTeX act more like what I was used to in KILE
;; (require 'tex )
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

;; maybe someone can help me with adding cref and Cref to TeX-add-style-hook
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

;; ares-mode and hook
(require 'ares-mode)
(add-to-list 'auto-mode-alist '("\\.ares$" . ares-mode))
(add-to-list 'auto-mode-alist '("\\.cale$" . ares-mode))
(add-hook 'ares-mode-hook
	  (lambda ()
	    (visual-line-mode t)))

;; Python Hook
(add-hook 'python-mode-hook
          (lambda ( )
	    (visual-line-mode t)
	    (require 'sphinx-doc)
	    (sphinx-doc-mode t)
	    (visual-line-mode)
	    (setq python-indent-offset 2)
	    (setq indent-tabs-mode nil)
	    (setq tab-width 2)))
;; (add-hook 'python-mode-hook 'jedi:setup )
(put 'downcase-region 'disabled nil)

;; Restructed Text Mode Hooks
;(require 'jinja2-rst-mode )
;(mmm-add-mode-ext-class 'rst-mode "\\.rst\\'" 'jinja2)
(add-hook 'rst-mode-hook
 	  (lambda ( )
 	    (require 'sphinx-mode)
 	    (visual-line-mode t)))

;; emojify, following instructions from https://github.com/iqbalansari/emacs-emojify#with-use-package
;; (add-hook 'after-init-hook 'global-emojify-mode)

;; got this unwrap-line emacs command from emacswiki.org/emacs/UnwrapLine
(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
	(end (copy-marker (or (search-forward "\n\n" nil t)
			      (point-max))))
	(fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))
