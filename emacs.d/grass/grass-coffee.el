(defun grass/indent-relative (&optional arg)
  "Newline and indent same number of spaces as previous line."
  (interactive)
  (let* ((indent (+ 0 (save-excursion
                        (back-to-indentation)
                        (current-column)))))
    (newline 1)
    (insert (make-string indent ?\s))))

(use-package coffee-mode
  :ensure t
  :init
  (progn
    ;; Proper indents when we evil-open-below etc...
    (defun grass/coffee-indent ()
      (if (coffee-line-wants-indent)
          ;; We need to insert an additional tab because the last line was special.
          (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
        ;; Otherwise keep at the same indentation level
        (coffee-insert-spaces (coffee-previous-indent))))

    ;; Override indent for coffee so we start at the same indent level
    (defun grass/coffee-indent-line ()
      "Indent current line as CoffeeScript."
      (interactive)
      (let* ((curindent (current-indentation))
             (limit (+ (line-beginning-position) curindent))
             (type (coffee--block-type))
             indent-size
             begin-indents)
        (if (and type (setq begin-indents (coffee--find-indents type limit '<)))
            (setq indent-size (coffee--decide-indent curindent begin-indents '>))
          (let ((prev-indent (coffee-previous-indent))
                (next-indent-size (+ curindent coffee-tab-width)))
            (if (= curindent 0)
                (setq indent-size prev-indent)
              (setq indent-size (+ curindent coffee-tab-width) ))
            (coffee--indent-insert-spaces indent-size)))))

    (add-hook 'coffee-mode-hook
              (lambda ()
                (set (make-local-variable 'tab-width) 2)
                (flycheck-mode t)
                (setq indent-line-function 'grass/coffee-indent-line)
                (setq evil-shift-width coffee-tab-width)))))

(provide 'grass-coffee)
