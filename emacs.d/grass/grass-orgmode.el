
(use-package org
  :defer t
  :bind ("C-c a" . org-agenda)

  :config
  ;; Make windmove work in org-mode
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  ;; Show indents
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-agenda-files '("~/Dropbox/Notes"))
  ;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil)

  ;; Use pandoc for exports
  (use-package ox-pandoc)

  ;; Create reveal js presentations in org mode.
  (use-package ox-reveal
    :init
    (setq org-reveal-root (concat "file://" (expand-file-name "~/Dropbox/Backups/Reveal/reveal.js")))
    ;; Use htmlize to highlight source code block using my emacs theme
    (use-package htmlize))

  (use-package org-mac-link
    :bind ("C-c g" . org-mac-grab-link))

  ;; Show raw link text
  (setq org-descriptive-links nil)
  ;; Start up fully open
  (setq org-startup-folded nil)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))

  ;; Allow bind in files to enable export overrides
  (setq org-export-allow-bind-keywords t)
  (defun grass/html-filter-remove-src-blocks (text backend info)
    "Remove source blocks from html export."
    (when (org-export-derived-backend-p backend 'html) ""))

  ;; Code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (ruby . t)
     (sh . t)))

  ;; Highlight source blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)

 (add-hook 'org-shiftup-final-hook 'windmove-up)
 (add-hook 'org-shiftleft-final-hook 'windmove-left)
 (add-hook 'org-shiftdown-final-hook 'windmove-down)
 (add-hook 'org-shiftright-final-hook 'windmove-right)

  (add-hook 'org-mode-hook
    (lambda ()
      ;; No auto indent please
      (setq evil-auto-indent nil)
      (setq org-export-html-postamble nil)
      ;; Let me keep my prefix key binding
      (define-key org-mode-map (kbd "C-,") nil)
      ;; (org-hide-block-all)
      ;; (define-key org-mode-map (kbd "C-c t") 'org-hide-block-toggle)
      (define-key org-mode-map (kbd "C-, a") 'org-cycle-agenda-files))))

(provide 'grass-orgmode)
