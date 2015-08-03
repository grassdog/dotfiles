
(use-package org
  :defer t
  :init

  ;; Start up fully open
  (setq org-startup-folded nil)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

                                        ; (setq org-todo-keywords '((sequence "TODO" "WAIT" "|" "DONE" "CANCELED")))

  ;; Make windmove work in org-mode
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-agenda-files '("~/Dropbox/Notes"))

  (add-hook 'org-mode-hook
    (lambda ()
      ;; No auto indent please
      (setq evil-auto-indent nil)
      ;; Let me keep my prefix key binding
      (define-key org-mode-map (kbd "C-,") nil)
      (define-key org-mode-map (kbd "C-, a") 'org-cycle-agenda-files))))

(provide 'grass-orgmode)
