(use-package web-mode
  :mode  (("\\.html?\\'"    . web-mode)
          ("\\.jsx$"        . web-mode)
          ("\\.erb\\'"      . web-mode)
          ("\\.ejs\\'"      . web-mode)
          ("\\.ect\\'"      . web-mode)
          ("\\.as[cp]x\\'"  . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.dhtml\\'"    . web-mode))
  :config
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
      (setq web-mode-enable-comment-keywords t)
      (setq web-mode-comment-style 2)     ;; Use server stye comments
      (global-set-key (kbd "C-, b") 'web-beautify-html)
      (global-set-key (kbd "C-, z") 'web-mode-fold-or-unfold)
      (setq web-mode-enable-current-element-highlight t))
    (add-hook 'web-mode-hook  'grass/web-mode-hook)))

;; Setup for jsx
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(use-package jade-mode
  :mode "\\.jade$"
  :init
  (require 'sws-mode)
  (require 'stylus-mode)
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))
  ;; (add-hook 'jade-mode-hook
  ;;           (lambda ()
  ;;             (highlight-indentation-mode t)))
  ;; (add-hook 'stylus-mode-hook
  ;;           (lambda ()
  ;;             (highlight-indentation-mode t)))
  )

(use-package scss-mode
  :mode "\\.scss$"
  :config
  (use-package rainbow-mode)
  (add-hook 'scss-mode-hook
            (lambda ()
              (linum-mode)
              (rainbow-mode +1)
              (setq evil-shift-width css-indent-offset))))

(use-package css-mode
  :mode "\\.css$"
  :config
  (use-package rainbow-mode)
  (add-hook 'css-mode-hook
            (lambda ()
              (linum-mode)
              (rainbow-mode +1)
              (global-set-key (kbd "C-, b") 'web-beautify-css))))

(use-package yaml-mode
  :defer t)

(use-package haml-mode
  :disabled t)

(use-package feature-mode
  :disabled t)

(provide 'grass-webmode)
