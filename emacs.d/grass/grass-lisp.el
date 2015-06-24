(defun grass/lisp-coding-defaults ()
  (paredit-mode +1)
  (rainbow-delimiters-mode +1))

(add-hook 'lisp-mode-hook 'grass/lisp-coding-defaults)
(add-hook 'elisp-mode-hook 'grass/lisp-coding-defaults)
(add-hook 'scheme-mode-hook 'grass/lisp-coding-defaults)

; TODO Get interactive elisp working
; (add-hook 'ielm-mode-hook 'grass/lisp-coding-defaults)

(provide 'grass-lisp)
