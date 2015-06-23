(defun grass/indent-relative (&optional arg)
  "Newline and indent same number of spaces as previous line."
  (interactive)
  (let* ((indent (+ 0 (save-excursion
                             (back-to-indentation)
                             (current-column)))))
    (newline 1)
    (insert (make-string indent ?\s))))

(require 'coffee-mode)

;; Proper indents when we evil-open-below etc...
(defun grass/coffee-indent ()
  (if (coffee-line-wants-indent)
    ;; We need to insert an additional tab because the last line was special.
    (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
    ;; Otherwise keep at the same indentation level
    (coffee-insert-spaces (coffee-previous-indent))))

(add-hook 'coffee-mode-hook
  (lambda ()
    (set (make-local-variable 'tab-width) 2)
    (setq indent-line-function 'grass/coffee-indent)
    (setq evil-shift-width coffee-tab-width)))

(provide 'grass-coffee)
