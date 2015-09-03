
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ahs-face ((t (:foreground "magenta" :background unspecified :slant normal :weight bold))))
  '(hl-sexp-face ((t (:background "#616161")))))

(if (display-graphic-p)
  (custom-set-faces
   '(hl-sexp-face ((t (:background "#073642"))))))

(custom-set-variables
  ; Set up hasktags
  '(haskell-tags-on-save nil)
  ; Set up interactive mode
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-suggest-remove-import-lines t))
