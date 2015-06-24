
;; Interactive list refinement
(require 'helm-config)
(require 'helm-projectile)

(setq helm-buffers-fuzzy-matching t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)

;; Force helm to always open from bottom
;; disable popwin-mode in an active Helm session It should be disabled
;; otherwise it will conflict with other window opened by Helm persistent
;; action, such as *Help* window.
(require 'popwin)
(popwin-mode 1)
(push '("^\*helm.+\*$" :regexp t :height 25) popwin:special-display-config)
(push '("^\*ag.+\*$" :regexp t :height 25) popwin:special-display-config)
(add-hook 'helm-after-initialize-hook (lambda ()
                                         (popwin:display-buffer helm-buffer t)
                                         (popwin-mode -1)))

;; Restore popwin-mode after a Helm session finishes.
(add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

(evil-leader/set-key "f" 'helm-find-files)

(evil-leader/set-key "o" 'helm-buffers-list)
(global-set-key (kbd "C-, o") 'helm-buffers-list)

(evil-leader/set-key "r" 'helm-recentf)
(global-set-key (kbd "C-, r") 'helm-recentf)

(global-set-key (kbd "M-x") 'helm-M-x)
;; Bind the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'grass-helm)
