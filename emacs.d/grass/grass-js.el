;; TODO Add coffee
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js2-mode-hook
 (lambda ()
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1)))

(provide 'grass-js)
