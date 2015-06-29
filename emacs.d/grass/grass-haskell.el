;; Install some useful packages so this all works
;; cabal update && cabal install happy hasktags stylish-haskell present ghc-mod hlint

(use-package haskell-mode
  :ensure t
  :init
  (progn
    ; Make Emacs look in Cabal directory for binaries
    (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
      (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
      (add-to-list 'exec-path my-cabal-path))

    ; Add F8 key combination for going to imports block
    (eval-after-load 'haskell-mode
      '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

    (setq haskell-indentation-disable-show-indentations t)
    (custom-set-variables
     ; Set up hasktags
     '(haskell-tags-on-save t)
     ; Set up interactive mode
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-process-suggest-remove-import-lines t)
     ; Set interpreter to be "cabal repl"
     '(haskell-process-type 'cabal-repl))

    ; Add key combinations for interactive haskell-mode
    (eval-after-load 'haskell-mode '(progn
                                      (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                      (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                      (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                      (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                      (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                      (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                      (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                      (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
    (eval-after-load 'haskell-cabal '(progn
                                       (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                       (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                       (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                       (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                       (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

    (eval-after-load 'haskell-mode
      '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
    (eval-after-load 'haskell-cabal
      '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))))


;; Use hi2 for indentation
(use-package hi2
  :ensure t
  :config
  (setq hi2-show-indentations nil)
  :init
  (add-hook 'haskell-mode-hook 'turn-on-hi2))

(use-package ghc
  :ensure t
  :disabled t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package company-ghc
  :ensure t
  :disabled t
  :init
  (add-to-list 'company-backends 'company-ghc) (custom-set-variables '(company-ghc-show-info t)))

(provide 'grass-haskell)
