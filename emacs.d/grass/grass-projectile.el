
;; Project file management
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq helm-projectile-fuzzy-match t)
  :init
  (progn
    (projectile-global-mode t)

    (add-to-list 'projectile-globally-ignored-directories "gems")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "dist")
    (add-to-list 'projectile-globally-ignored-directories "elpa")

    (global-set-key (kbd "C-, C-p") 'helm-projectile)
    (global-set-key (kbd "C-, p") 'helm-projectile)
    (helm-projectile-on)))

(provide 'grass-projectile)
