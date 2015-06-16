
;; Start up open and indented nicely
(setq org-startup-folded nil)

(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(evil-leader/set-key-for-mode 'org-mode "t" 'org-todo)

(add-hook 'org-mode-hook
 (lambda ()
   ;; No auto indent please
   (setq evil-auto-indent nil)))

(provide 'grass-orgmode)
