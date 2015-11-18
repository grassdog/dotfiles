
;; Interactive list refinement
(use-package helm
  :diminish helm-mode
  :bind (("C-, o" . helm-buffers-list)
         ("C-, r" . helm-recentf)
         ("C-, C-f" . helm-find-files)
         ("M-x" . helm-M-x))
  :config
  (require 'helm-config)

  (define-key helm-map (kbd "C-, l")  'helm-select-action)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; Make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

  ;; Show full buffer names please
  (setq helm-buffer-max-length 40)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-split-window-in-side-p t)

  ;; Echo our typing in helm header so it's easier to see
  (setq helm-display-header-line t)
  (setq helm-echo-input-in-header-line t)

  (use-package helm-fuzzier
    :init
    (helm-fuzzier-mode 1))

  (use-package helm-flx
    :init
    (helm-flx-mode +1))

  (use-package helm-ag
    :commands helm-ag
    ;;:config
    ;; Prepopulate search with the symbol under point
    ;;(setq helm-ag-insert-at-point 'symbol)
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
