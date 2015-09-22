
(use-package projectile
  :diminish projectile-mode
  :config
  (progn

  (use-package helm-projectile
    :bind (("C-, C-p" . helm-projectile)
          ("C-, p" . helm-projectile-find-file))
    :init
      (helm-projectile-on))

    (setq projectile-tags-command "getags")
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'helm)
    (setq helm-projectile-fuzzy-match t)
    ;; Show unadded files also
    (setq projectile-hg-command "( hg locate -0 -I . ; hg st -u -n -0 )")

    (add-to-list 'projectile-globally-ignored-directories "gems")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "bower_components")
    (add-to-list 'projectile-globally-ignored-directories "dist")
    (add-to-list 'projectile-globally-ignored-directories "/emacs.d/elpa/")
    (add-to-list 'projectile-globally-ignored-files ".keep")
    (add-to-list 'projectile-globally-ignored-files "TAGS")
    (projectile-global-mode t)))

(provide 'grass-projectile)
