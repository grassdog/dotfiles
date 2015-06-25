
(evil-leader/set-key "d" 'dired-jump)
(evil-leader/set-key "e" 'pp-eval-last-sexp)
(evil-leader/set-key "f" 'ido-find-file)
(evil-leader/set-key "t" 'ido-goto-symbol)
(evil-leader/set-key "k" 'kill-this-buffer)


(evil-leader/set-key "y" 'bury-buffer)
(evil-leader/set-key "s" 'occur)

;; Move lines
(global-set-key (kbd "<C-S-up>") 'grass/move-line-up)
(global-set-key (kbd "<C-S-down>")  'grass/move-line-down)

(global-set-key (kbd "C-c u") 'grass/view-url)
(global-set-key (kbd "C-c r") 'grass/indent-region-or-buffer)

; (global-set-key (kbd "C-w =") 'balance-windows)

;; Switch buffers, even in non evil modes
(evil-leader/set-key "," 'grass/switch-to-previous-buffer)
(global-set-key (kbd "C-, ,") 'grass/switch-to-previous-buffer)

;; Helm
(evil-leader/set-key "o" 'helm-buffers-list)
(global-set-key (kbd "C-, o") 'helm-buffers-list)

(evil-leader/set-key "r" 'helm-recentf)
(global-set-key (kbd "C-, r") 'helm-recentf)

(global-set-key (kbd "M-x") 'helm-M-x)

;; Projectile
(global-set-key (kbd "C-, p") 'helm-projectile)
(evil-leader/set-key "p p" 'helm-projectile)
(evil-leader/set-key "p a" 'projectile-ag)
(evil-leader/set-key "p b" 'projectile-switch-to-buffer)
(evil-leader/set-key "p D" 'projectile-dired)
(evil-leader/set-key "p d" 'projectile-find-dir)
(evil-leader/set-key "p e" 'project-explorer-open)
(evil-leader/set-key "p t" 'projectile-find-tag)
(evil-leader/set-key "p k" 'projectile-kill-buffers)
(evil-leader/set-key "p R" 'projectile-regenerate-tags)
(evil-leader/set-key "p r" 'helm-projectile-recentf)
(evil-leader/set-key "p s" 'helm-projectile-switch-project)

;; Org
(evil-leader/set-key-for-mode 'org-mode "t" 'org-todo)
(evil-leader/set-key-for-mode 'org-mode "a" 'org-todo-list)
(evil-leader/set-key-for-mode 'org-mode "l" 'org-insert-link)


;; Bind the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Up in dired
(add-hook 'dired-mode-hook
          (lambda () (local-set-key (kbd "U") 'dired-up-directory)))

(provide 'grass-keybindings)
