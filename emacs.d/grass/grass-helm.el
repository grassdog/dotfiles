
;; Interactive list refinement
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    ;; Show full buffer names please
    (setq helm-buffer-max-length 40)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-M-x-fuzzy-match t)
    (setq helm-split-window-in-side-p t)
    (add-to-list 'recentf-exclude "\\ido.hist\\'")
    (add-to-list 'recentf-exclude "\\TAGS\\'")
    (helm-mode 1)
    ;; Helm
    (global-set-key (kbd "C-, r") 'helm-recentf)
    (global-set-key (kbd "C-, o") 'helm-buffers-list)

    (global-set-key (kbd "M-x") 'helm-M-x)
    ;; Bind the old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package helm-projectile
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package helm-swoop
  :ensure t
  :init
  (global-set-key (kbd "C-, s") 'helm-swoop))

;; Force helm to always open at the bottom
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(provide 'grass-helm)
