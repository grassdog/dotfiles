(use-package js2-mode
  :mode  "\\.js$"
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
    (setq js2-strict-trailing-comma-warning nil)

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

(use-package json-mode
  :mode "\\.json$"
  :bind ("C-, b" . json-reformat)
  :config
  (use-package flymake-json
    :init
    (add-hook 'json-mode 'flymake-json-load))

  (flycheck-mode 1))

(use-package tide
  :disabled
  :mode "\\.ts$"
  :config
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2)
  (add-hook 'typescript-mode-hook
     (lambda ()
       (tide-setup)
       (flycheck-mode t)
       (setq flycheck-check-syntax-automatically '(save mode-enabled))

       ;; aligns annotation to the right hand side
       ;; (setq company-tooltip-align-annotations t)
       (eldoc-mode nil))))

(use-package typescript-mode
  :mode "\\.ts$"
  :config
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 2)
  (use-package tss
    :config
    ;(setq tss-popup-help-key "C-:")
    ;(setq tss-jump-to-definition-key "C->")
    ;(setq tss-implement-definition-key "C-c i")
    (tss-config-default)))

(use-package elm-mode
  :mode "\\.elm$"
  :config

  (use-package flycheck-elm
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

  (add-hook 'elm-mode-hook
    (lambda ()
      ;; Reenable elm oracle once it's start up cost doesn't smash editor performance
      ;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

      (setq elm-indent-offset 2)
      (setq evil-shift-width 2)
      (elm-indent-mode -1) ;; This is getting in the way more than not at the moment
      (flycheck-mode t))))


(provide 'grass-js)
