
;; Override background colour to work better with Solarized
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(hl-sexp-face ((t (:background "#073642"))))
  '(tabbar-selected-modified ((t :inherit tabbar-selected)))
  '(tabbar-unselected-modified ((t :inherit tabbar-unselected)))
  '(tabbar-unselected-modified ((t :inherit tabbar-unselected))))

(custom-set-variables
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t))
