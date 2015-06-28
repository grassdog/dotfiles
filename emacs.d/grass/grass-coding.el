(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(use-package company
  :ensure t
  :init
    (setq company-idle-delay 0.4)
    (add-hook 'after-init-hook 'global-company-mode)
  :config
    (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
        (company-complete-common)
        (indent-according-to-mode))))

(use-package web-beautify
  :ensure t)

;; Line numbers for coding please
(setq on-console (null window-system))
(setq linum-format (if on-console "%4d " "%4d"))

(add-hook 'prog-mode-hook
  (lambda () (linum-mode 1)))

;; Show current function in modeline
(which-function-mode)

(use-package puppet-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package python
  :ensure t)

(provide 'grass-coding)
