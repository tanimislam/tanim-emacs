;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)
(package-initialize)

;; no start up message
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; removes *messages* from the buffer
(setq-default message-log-max nil)


;; set default font
;; (set-frame-font "Liberation Mono 11")

;; add elisp to path
(add-to-list 'load-path "~/.xemacs")
(add-to-list 'load-path "~/.xemacs/elisp")

;; default height and width of frame
(add-to-list 'default-frame-alist '(height . 45 ) '(width . 80))
(add-to-list 'initial-frame-alist '(height . 45 ) '(width . 80))

;; column-number-mode
(setq column-number-mode t)

;; turn on font-lock mode
(global-font-lock-mode)

;; (require 'wpdl-mode)
;; (require 'mercury-mode)

;; enable visual feedback on selections
(setq transient-mark-mode t)
(setq backup-inhibited t)

;; now auto-load the following
(autoload 'awk-mode "cc-mode" nil t)

;; autoload matlab-mode
(require 'matlab)
(autoload 'matlab-mode "matlab" "Enter matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; systemd-mode
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; now load up css-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode ) )
(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode ) )
(setq cssm-indent-function 'cssm-c-style-indenter)

;; now setting up display to the following
(mouse-wheel-mode 1)

; (require 'font-latex)
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

;; add in the packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


;; dracula
(load-theme 'dracula t)

;; ares-mode
;; (require 'ares-mode)
;; (add-to-list 'auto-mode-alist '("\\.ares$" . ares-mode))

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

;; swig-mode
(require 'swig-mode)
(add-to-list 'auto-mode-alist '("\\.sw\\'" . swig-mode))

;; autoload matlab-mode
(require 'matlab)
(autoload 'matlab-mode "matlab" "Enter matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; systemd-mode
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; now load up css-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode ) )
(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode ) )
(setq cssm-indent-function 'cssm-c-style-indenter)

;; Python Hook
(add-hook 'python-mode-hook
          (lambda ( )
	    (setq indent-tabs-mode nil)
	    (setq tab-width 2)))
;; (add-hook 'python-mode-hook 'jedi:setup )
(put 'downcase-region 'disabled nil)

;; inherit in the $PATH from the shell
(exec-path-from-shell-initialize)
