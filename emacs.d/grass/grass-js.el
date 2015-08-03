(use-package js2-mode
  :ensure t
  :mode  (("\\.js$"   . js2-mode)
          ("\\.json$" . js2-mode))
  :interpreter "node"
  :init
    (add-hook 'js2-mode-hook
    (lambda ()
        (setq mode-name "JS2")
        (setq js2-global-externs '("require" "module" "jest" "jasmine"
                                    "it" "expect" "describe" "beforeEach"))
        (setq evil-shift-width js-indent-level)
        (subword-mode +1)
        (global-set-key (kbd "C-, F") 'web-beautify-js)
        (js2-imenu-extras-mode +1))))

(provide 'grass-js)
