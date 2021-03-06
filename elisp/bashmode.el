;; move cursor to the previous line or get previous history item,
;; depending on whether we're at a shell mode prompt
(defun ewd-comint-up (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input arg)
    (previous-line arg)))

;; move cursor to the next line or get next history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-comint-down (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input arg)
    (next-line arg)))

;; bind my special functions to the up and down keys in shell mode
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map [up] 'ewd-comint-up)
            (define-key shell-mode-map [down] 'ewd-comint-down)))
