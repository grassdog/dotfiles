
(evil-leader/set-key "d" 'dired-jump)
(evil-leader/set-key "e" 'pp-eval-last-sexp)

;; Move lines
;; TODO Apply this in a hook
(global-set-key (kbd "<C-S-up>") 'grass/move-line-up)
(global-set-key (kbd "<C-S-down>")  'grass/move-line-down)

;; Create file in dired
(evil-leader/set-key-for-mode 'dired-mode "c" 'find-file)

(provide 'grass-keybindings)
