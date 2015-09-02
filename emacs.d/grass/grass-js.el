(use-package js2-mode
  :mode  (("\\.js$"   . js2-mode)
          ("\\.json$" . js2-mode))
  :interpreter "node"
  :config
    (use-package js2-refactor
      :init
      (add-hook 'js2-mode-hook #'js2-refactor-mode)
      (js2r-add-keybindings-with-prefix "C-c RET"))

    ;; Rely on flycheck instead...
    (setq js2-show-parse-errors nil)
    ;;(setq js2-strict-missing-semi-warning nil)
    ;; jshint does not warn about this now for some reason
    (setq js2-strict-trailing-comma-warning t)

    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
    (add-hook 'js2-mode-hook
      (lambda ()
        (setq mode-name "JS2")
        (setq js2-global-externs '("module" "require" "buster" "jestsinon" "jasmine" "assert"
                                  "it" "expect" "describe" "beforeEach"
                                  "refute" "setTimeout" "clearTimeout" "setInterval"
                                  "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq evil-shift-width js-indent-level)


        (flycheck-mode 1)
        (global-set-key (kbd "C-, b") 'web-beautify-js)
        (js2-imenu-extras-mode +1))))

(use-package typescript-mode
  :mode "\\.ts$"
  :config
  (use-package tss
    :config
    ;(setq tss-popup-help-key "C-:")
    ;(setq tss-jump-to-definition-key "C->")
    ;(setq tss-implement-definition-key "C-c i")
    (tss-config-default)))

(use-package elm-mode
  :mode "\\.elm$")

(provide 'grass-js)
