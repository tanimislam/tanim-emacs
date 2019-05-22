;; Emacs Major mode for Ares input decks
;; Michael E. Wickett, wickett@llnl.gov
;; 09-08-2009

;; Provides syntax highlighting and basic indentation.
;;
;; Developed with GNU Emacs, but should work with XEmacs.
;;
;; Put this file somewhere on your load-path, or put it in .emacs.d and use
;;    (setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
;; Then add (require 'ares-mode) in your init file.
;;
;; To use a higher level of syntax highlighting, use
;;    (setq ares-font-lock-keywords ares-font-lock-keywords-3)
;; To use a lower level, use
;;    (setq ares-font-lock-keywords ares-font-lock-keywords-1)
;;
;; Other customizations include setting different indent levels by
;;    (setq ares-indent-offset 2)
;; or
;;    (setq ares-minimum-indent 0)

(defvar ares-indent-offset 3)   ;; indentation of if/do/cyc/etc blocks
(defvar ares-minimum-indent 3)  ;; min indentation of all deck commands

(defvar ares-mode-hook nil)

(defvar ares-mode-map
  (let ((ares-mode-map (make-keymap)))
    (define-key ares-mode-map "\C-j" 'newline-and-indent)
    ares-mode-map)
  "Keymap for ARES major mode")

(defconst ares-font-lock-keywords-1
  (list
   '("\\<\\(\\(ifun\\)?def\\|deck\\|cyc\\|while\\|end\\(msg\\|if\\|while\\|python\\|func\\|loop\\)?\\|do\\|done\\(set\\)?\\|if\\|else\\|elseif\\|atexit\\|python\\|once\\|name\\|func\\|level\\|create\\|delete\\|shape\\|loop\\(levels\\)?\\|set\\|eval\\)\\>" . font-lock-keyword-face)
  "Nominal highlighting expressions for ARES mode"))

(defconst ares-font-lock-keywords-2
  (append ares-font-lock-keywords-1
     (list
      '("{[^}]*}" (0 font-lock-variable-name-face t))
      '("\\<\\(\\(match\\)?bnd\\|species\\|spline\\|\\(p\\)?\\(re\\)?gen\\|\\(sub\\)?cycle\\(plot\\)?\\|\\(t\\)?plot\\|timeplot\\|pamp\\|tracer\\|material\\|reg\\(ion\\|op\\)\\|\\(end\\)?regopall\\|eos\\(op\\)?\\|particulates\\|blocks\\|block\\|quad\\|line\\(out\\)?\\|\\(v\\)?fill\\(var\\)?\\|zone\\|\\(node\\)?list\\|bc\\|\\(show\\)?\\(ale\\|amr\\)\\(off\\)?\\|gridmv\\|\\(show\\)?src\\(off\\)?\\|echo\\|\\(re\\)?blkdiv\\|pl\\|\\(w\\|r\\)dmp\\|open\\|close\\|quit\\|run\\|step\\|exit\\|ptc\\|time\\(rs\\)?\\|sphere\\|rect\\|ellips\\(e\\|oid\\)\\cyl\\(inder\\)?\\|ball\\|spharm\\|rlin\\|tet\\|proe\\|cone\\|data\\|polar\\|rdifit\\|regrid\\|amr_setup\\|\\(global\\|end\\)?refine\\|criterion\\|\\(amr\\|end\\)_per_level\\|\\(amr\\|end\\)_before_regrid\\|onedsetup\\|slide\\|tpamp\\|heburn\\|cascade\\|lund\\|\\(shad\\|det\\)vel\\|\\(n\\)?dets\\|det\\|axis\\|tv\\|fm\\|pl\\(c\\|m\\|b\\|i\\|v\\)\\|norm\\|deffield\\|cond\\(on\\|list\\|off\\|invoke\\|show\\)?\\|printconds\\|shell\\|component\\|opacity\\(op\\)?\\|at\\(exit\\|completion\\|restart\\|location\\)\\|\\(end\\)?setspec\\|mixgroup\\)\\>" . font-lock-function-name-face)
      '("\\<[0-9.]+[de]?[\\-+]?[0-9.]*\\>" . font-lock-constant-face)))
  "Standard highlighting expresssions for ARES mode")

(defconst ares-font-lock-keywords-3
  (append ares-font-lock-keywords-2
     (list
      '("set[ \t]*\\([^ \t]*\\)[ \t]" (1 font-lock-type-face))))
  "Extra highlighting expresssions for ARES mode")

(defvar ares-font-lock-keywords ares-font-lock-keywords-2
  "Default highlighting expressions for ARES mode")

(defvar ares-full-line-comment-regexp "^ *\\*.*$")
(defvar ares-startblock-regexp "^[ \t]*\\<\\(if\\|cyc\\(leplot\\)?\\|do\\|while\\|at\\(exit\\|completion\\|restart\\|location\\)\\|python\\|once\\|func\\|shape\\|sphere\\|rect\\|ellips\\(e\\|oid\\)\\cylinder\\|spharm\\|rlin\\|tet\\|proe\\|cone\\|data\\|polar\\|else\\|loop\\(levels\\)?\\|amr_setup\\|amr_per_level\\|amr_before_regrid\\|refine\\|setspec\\|regopall\\)\\>")
(defvar ares-endblock-regexp "^[ \t]*\\(end\\(msg\\|python\\|func\\|while\\|if\\|loop\\|_per_level\\|_before_regrid\\|refine\\|setspec\\|regopall\\)?\\|done\\|else\\)\\>")
(defvar ares-blank-line-regexp "^[ \t]*$")

(defun ares-indent-line ()
  "Indent current line as ARES code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check for rule beginning of buffer
      (if (not (looking-at ares-full-line-comment-regexp))
          (indent-line-to ares-minimum-indent))
    (let ((not-indented t) cur-indent)
      (if (looking-at ares-endblock-regexp) ; Check for end of block
          (progn
            (save-excursion
              (forward-line -1)
              (while (looking-at ares-blank-line-regexp) (forward-line -1))
              (setq cur-indent (- (current-indentation) ares-indent-offset)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (while (and (not (bobp)) (looking-at ares-blank-line-regexp)) (forward-line -1))
            (if (looking-at ares-endblock-regexp)
                (if (looking-at ares-startblock-regexp)  ;; such as an else
                    (progn
                      (setq cur-indent (+ (current-indentation) ares-indent-offset))
                      (setq not-indented nil))
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil)))
              (if (looking-at ares-startblock-regexp)
                  (progn
                    (setq cur-indent (+ (current-indentation) ares-indent-offset))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (if (not (looking-at ares-full-line-comment-regexp))
            (indent-line-to ares-minimum-indent))) ; If we didn't see an indentation hint, then allow no indentation
      )))

(defvar ares-mode-syntax-table
  (let ((ares-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\_ "w" ares-mode-syntax-table)
    (modify-syntax-entry ?\( "()" ares-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" ares-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" ares-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" ares-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" ares-mode-syntax-table)
    (modify-syntax-entry ?\} "){" ares-mode-syntax-table)
    (modify-syntax-entry ?\" "\"" ares-mode-syntax-table)
    (modify-syntax-entry ?\' "\"" ares-mode-syntax-table)
    (modify-syntax-entry ?\% "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\& "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\* "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\+ "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\- "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\/ "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\< "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\= "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\> "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\| "."  ares-mode-syntax-table)
    (modify-syntax-entry ?\* "<"  ares-mode-syntax-table)
    (modify-syntax-entry ?\n ">"  ares-mode-syntax-table)
    ares-mode-syntax-table)
  "Syntax table for ares-mode")

(define-derived-mode ares-mode fundamental-mode "Ares"
  "Major mode for editing ARES input decks."
;  :syntax-table ares-mode-syntax-table
  (make-local-variable 'ares-indent-offset)
  (make-local-variable 'ares-minimum-indent)
  (set (make-local-variable 'comment-start) "\* ")
;  (set (make-local-variable 'comment-start-skip) "\\*+[ \t]*")
  (set (make-local-variable 'font-lock-defaults) '(ares-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'ares-indent-line)
  (font-lock-mode)
  (run-hooks 'ares-mode-hook))

(provide 'ares-mode)
