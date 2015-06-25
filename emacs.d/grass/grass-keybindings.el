
(evil-leader/set-key "d" 'dired-jump)
(evil-leader/set-key "e" 'pp-eval-last-sexp)
(evil-leader/set-key "f" 'ido-find-file)

;; Move lines
(global-set-key (kbd "<C-S-up>") 'grass/move-line-up)
(global-set-key (kbd "<C-S-down>")  'grass/move-line-down)

(global-set-key (kbd "C-c u") 'grass/view-url)
(global-set-key (kbd "C-c r") 'grass/indent-region-or-buffer)

(add-hook 'dired-mode-hook
          (lambda () (local-set-key (kbd "U") 'dired-up-directory)))

(provide 'grass-keybindings)
