
(use-package projectile
  :diminish projectile-mode
  :config
  (progn

  (use-package helm-projectile
    :bind (("C-, C-p" . helm-projectile)
          ("C-, p" . helm-projectile))
    :init
      (helm-projectile-on))

    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'helm)
    (setq helm-projectile-fuzzy-match t)

    (add-to-list 'projectile-globally-ignored-directories "gems")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "dist")
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-files ".keep")
    (projectile-global-mode t)))

(provide 'grass-projectile)
