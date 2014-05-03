
;; Like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; mimic popular IDEs binding, note that it doesn't work in a terminal session
(global-set-key (kbd "<S-return>") 'grass-insert-empty-line-below)
(global-set-key (kbd "<C-S-return>") 'grass-insert-empty-line-above)

(global-set-key (kbd "<C-S-up>") 'grass-move-line-up)
(global-set-key (kbd "<C-S-down>")  'grass-move-line-down)
(global-set-key (kbd "C-c n") 'grass-cleanup-buffer)
(global-set-key (kbd "C-M-\\") 'grass-indent-region-or-buffer)
(global-set-key (kbd "C-c u") 'grass-view-url)
(global-set-key (kbd "C-c s") 'grass-swap-windows)
(global-set-key (kbd "C-c d") 'grass-duplicate-current-line-or-region)
(global-set-key (kbd "C-c r") 'grass-rename-file-and-buffer)
(global-set-key (kbd "C-c k") 'grass-kill-other-buffers)
(global-set-key (kbd "C-c h") 'helm-prelude)


;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)

;; a complement to the zap-to-char command, that doesn't eat up the target character
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c w") (make-repeatable-command 'er/expand-region))

;; make C-x C-x usable with transient-mark-mode
(define-key global-map
  [remap exchange-point-and-mark]
  'grass-exchange-point-and-mark)


(provide 'grass-keybindings)
