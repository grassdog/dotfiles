(use-package web-mode
  :ensure t
  :mode  (("\\.html?\\'"    . web-mode)
          ("\\.jsx$"        . web-mode)
          ("\\.erb\\'"      . web-mode)
          ("\\.as[cp]x\\'"  . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.dhtml\\'"    . web-mode))
  :init
  (progn

    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))

    (defun grass/web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq evil-shift-width 2)
      (setq web-mode-enable-current-element-highlight t))
      (define-key evil-normal-state-map "za" 'web-mode-fold-or-unfold)
    (add-hook 'web-mode-hook  'grass/web-mode-hook)))

(use-package scss-mode
  :ensure t
  :init
  (add-hook 'scss-mode-hook
            (lambda ()
              (setq evil-shift-width css-indent-offset))))

(use-package css-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :ensure t
  :disabled t)

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'scss-mode-hook
            (lambda ()
              (rainbow-mode +1)))
  (add-hook 'css-mode-hook
            (lambda ()
              (rainbow-mode +1))))

(use-package feature-mode
  :ensure t
  :disabled t)

(provide 'grass-webmode)
