

(global-set-key (kbd "C-M-h") 'backward-kill-word) ; Like Readline.

(global-set-key (kbd "C-c h") 'helm-prelude)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; replace buffer-menu with ibuffer
; (global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)

;; make C-x C-x usable with transient-mark-mode
(define-key global-map
  [remap exchange-point-and-mark]
  'grass-exchange-point-and-mark)


(provide 'grass-keybindings)
