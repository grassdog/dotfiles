
;; Trojan horse maneuver

;; Leaders
(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil)
(evil-mode t)

(provide 'grass-evil)
