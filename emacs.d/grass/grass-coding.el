(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(use-package company
  :ensure t
  :diminish company-mode
  :init
    (setq company-idle-delay 0.3)
    (setq company-minimum-prefix-length 2)
    (add-hook 'after-init-hook 'global-company-mode)
  :config
    (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
        (company-complete-common)
        (indent-according-to-mode))))

(use-package web-beautify
  :ensure t
  :init
  ;; Clean files
  ;(evil-leader/set-key-for-mode 'web-mode "c" 'web-beautify-html)
  ;(evil-leader/set-key-for-mode 'css-mode "c" 'web-beautify-css)
  (evil-leader/set-key "i" 'grass/indent-region-or-buffer)
  (evil-leader/set-key-for-mode 'js2-mode "c" 'web-beautify-js))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

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
