
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

    ;; Projectile
    (global-set-key (kbd "C-, p") 'helm-projectile)
    (evil-leader/set-key "p p" 'helm-projectile)
    (evil-leader/set-key "p a" 'projectile-ag)
    (evil-leader/set-key "p r" 'projectile-replace)
    (evil-leader/set-key "p s" 'helm-projectile-switch-project)
    (evil-leader/set-key "p b" 'projectile-switch-to-buffer)
    (evil-leader/set-key "p D" 'projectile-dired)
    (evil-leader/set-key "p d" 'projectile-find-dir)
    (evil-leader/set-key "p e" 'project-explorer-open)
    (evil-leader/set-key "p t" 'projectile-find-tag)
    (evil-leader/set-key "p k" 'projectile-kill-buffers)
    (evil-leader/set-key "p T" 'projectile-regenerate-tags)
    (evil-leader/set-key "p R" 'helm-projectile-recentf)
    (helm-projectile-on)))

(provide 'grass-projectile)
