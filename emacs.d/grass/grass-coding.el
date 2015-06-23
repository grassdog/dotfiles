(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(require 'auto-complete)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" 'ac-complete)

(provide 'grass-coding)
