(use-package js2-mode
  :mode  (("\\.js$" . js2-mode)
          ("\\.es6$" . js2-mode))
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
  :bind ("C-, b" . json-pretty-print-buffer)
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

;; Flow
(define-derived-mode flow-mode typescript-mode "Flow"
  "JavaScript with Flow type checking")
;; (define-key flow-mode-map (kbd ":") nil)
;;(add-to-list 'auto-mode-alist '("\\.jsx$" . flow-mode))

;; (use-package f)
;; (require 'json)
;; (defun flycheck-parse-flow (output checker buffer)
;;   (let ((json-array-type 'list))
;;     (let ((o (json-read-from-string output)))
;;       (mapcar #'(lambda (errp)
;;                   (let ((err (cadr (assoc 'message errp))))
;;                     (flycheck-error-new
;;                      :line (cdr (assoc 'line err))
;;                      :column (cdr (assoc 'start err))
;;                      :level 'error
;;                      :message (cdr (assoc 'descr err))
;;                      :filename (f-relative
;;                                 (cdr (assoc 'path err))
;;                                 (f-dirname (file-truename
;;                                             (buffer-file-name))))
;;                      :buffer buffer
;;                      :checker checker)))
;;               (cdr (assoc 'errors o))))))

;; (flycheck-define-checker javascript-flow
;;   "Static type checking using Flow."
;;   :command ("flow" "--json" source-original)
;;   :error-parser flycheck-parse-flow
;;   :modes flow-mode)
;; (add-to-list 'flycheck-checkers 'javascript-flow)

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

      ;; This is getting in the way more than not at the moment
      (elm-indent-mode -1)
      (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1))
      (setq evil-shift-width 4)
      (setq tab-width 4)

      ;; TODO Custom indent function
      ;; - Look at previous line level
      ;; - If it is one of the indenting words/characters add two to level
      ;; - Steal from indent-relative
      ;; (setq indent-line-function 'grass/elm-indent-line)

      (define-key elm-mode-map [backtab] 'evil-shift-left-line)
      (flycheck-mode t))))


(provide 'grass-js)
