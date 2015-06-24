
(evil-leader/set-key "d" 'dired-jump)
(evil-leader/set-key "e" 'pp-eval-last-sexp)

;; Move lines
(global-set-key (kbd "<C-S-up>") 'grass/move-line-up)
(global-set-key (kbd "<C-S-down>")  'grass/move-line-down)

(add-hook 'dired-mode-hook
          (lambda () (local-set-key (kbd "U") 'dired-up-directory)))

(provide 'grass-keybindings)
