
;; Interactive list refinement
(use-package helm
  :diminish helm-mode
  :bind (("C-, o" . helm-buffers-list)
         ("C-, r" . helm-recentf)
         ("M-x" . helm-M-x))
  :config
    (require 'helm-config)
    ;; Show full buffer names please
    (setq helm-buffer-max-length 40)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-M-x-fuzzy-match t)
    (setq helm-split-window-in-side-p t)

    (use-package helm-ag
      :commands helm-ag
      ;; (setq helm-ag-insert-at-point 'symbol)
      )

    (helm-mode 1))

(use-package helm-swoop
  :bind ("C-, s s" . helm-swoop))

;; Force helm to always open at the bottom
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.5)))

(use-package helm-flyspell
  :commands helm-flyspell-correct
  :bind ("C-, S c" . helm-flyspell-correct))


(provide 'grass-helm)
