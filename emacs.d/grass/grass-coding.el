(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\|DEBUG\\)"
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
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Line numbers for coding please
(setq on-console (null window-system))
(setq linum-format (if on-console "%4d " "%4d"))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (add-hook 'enh-ruby-mode-hook #'smartparens-mode)
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode))

(require 'hideshow)
(diminish 'hs-minor-mode)

(add-hook 'prog-mode-hook
  (lambda ()
    (linum-mode 1)
    (hs-minor-mode t)))

;; Show current function in modeline
(which-function-mode)

(global-set-key (kbd "C-, i") 'imenu)

(use-package puppet-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package python
  :ensure t)

(provide 'grass-coding)
