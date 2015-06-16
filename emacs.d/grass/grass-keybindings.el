
(evil-leader/set-key "ff" 'dired-jump)
(evil-leader/set-key "e" 'pp-eval-last-sexp)

;; Create file in dired
(evil-leader/set-key-for-mode 'dired-mode "c" 'find-file)

(provide 'grass-keybindings)
