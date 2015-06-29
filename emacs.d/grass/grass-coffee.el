(use-package coffee-mode
  :ensure t
  :init
  (progn
    (add-hook 'coffee-mode-hook
              (lambda ()
                (set (make-local-variable 'tab-width) 2)
                (setq evil-shift-width coffee-tab-width)))))

(provide 'grass-coffee)
