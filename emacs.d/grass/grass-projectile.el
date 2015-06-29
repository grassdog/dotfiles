
;; Project file management
(use-package projectile
  :ensure t
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

    ;; Projectile
    (global-set-key (kbd "C-, p") 'helm-projectile)
    (evil-leader/set-key "p"   'helm-projectile)
    (evil-leader/set-key "a" 'projectile-ag)
    (evil-leader/set-key "t p" 'helm-projectile)
    (evil-leader/set-key "t r" 'projectile-replace)
    (evil-leader/set-key "t s" 'helm-projectile-switch-project)
    (evil-leader/set-key "t b" 'projectile-switch-to-buffer)
    (evil-leader/set-key "t d" 'projectile-find-dir)
    (evil-leader/set-key "t D" 'projectile-dired)
    (evil-leader/set-key "t t" 'projectile-find-tag)
    (evil-leader/set-key "t k" 'projectile-kill-buffers)
    (evil-leader/set-key "t T" 'projectile-regenerate-tags)
    (evil-leader/set-key "t R" 'helm-projectile-recentf)
    (helm-projectile-on)))

(provide 'grass-projectile)
