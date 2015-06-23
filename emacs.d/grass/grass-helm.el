
;; Interactive list refinement
(require 'helm-config)
(require 'helm-projectile)
(require 'shackle)

(setq helm-buffers-fuzzy-matching t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)

;; TODO Confirm this works
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))

(evil-leader/set-key "f" 'helm-find-files)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "r" 'helm-recentf)

(provide 'grass-helm)
