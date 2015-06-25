
;; Project file management
(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode t)

(add-to-list 'projectile-globally-ignored-directories "gems")
(add-to-list 'projectile-globally-ignored-directories "node_modules")

(setq projectile-completion-system 'helm)
(setq helm-projectile-fuzzy-match t)
(helm-projectile-on)

(provide 'grass-projectile)
