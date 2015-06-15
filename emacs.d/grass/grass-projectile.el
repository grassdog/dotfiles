
;; Project file management
(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode t)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(evil-leader/set-key "p" 'projectile-find-file)
(evil-leader/set-key "a" 'helm-projectile-ag)

(provide 'grass-projectile)
