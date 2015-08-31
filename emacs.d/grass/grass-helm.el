
;; Interactive list refinement
(use-package helm
  :diminish helm-mode
  :bind (("C-, o" . helm-buffers-list)
         ("C-, r" . helm-recentf)
         ("M-x" . helm-M-x))
  :config
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

    (use-package helm-ag
      :init
      ;; (setq helm-ag-insert-at-point 'symbol)
      )

    (helm-mode 1)))

(use-package helm-swoop
  :bind ("C-, s s" . helm-swoop))

;; Force helm to always open at the bottom
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.5)))

(provide 'grass-helm)
