;; Install some useful packages so this all works
;; cabal update && cabal install happy hasktags stylish-haskell present ghc-mod hlint

(use-package haskell-mode
  :defer t
  :config
  (progn

    ;; Use hi2 for indentation
    (use-package hi2
      :config
      (setq hi2-show-indentations nil)
      (add-hook 'haskell-mode-hook 'turn-on-hi2))

    (use-package ghc
      :config
      (autoload 'ghc-init "ghc" nil t)
      (autoload 'ghc-debug "ghc" nil t)
      (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

    (use-package company-ghc
      :disabled t
      :config
      (add-to-list 'company-backends 'company-ghc) (custom-set-variables '(company-ghc-show-info t)))

    ; Make Emacs look in Cabal directory for binaries
    (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
      (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
      (add-to-list 'exec-path my-cabal-path))

    ; Add F8 key combination for going to imports block
    (eval-after-load 'haskell-mode
      '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

    (setq haskell-indentation-disable-show-indentations t)

    ; Set interpreter to be "stack ghci"
    (setq haskell-process-type 'ghci)
    (setq haskell-process-path-ghci "stack")
    (setq haskell-process-args-ghci '("ghci"))
    (setq tab-always-indent t)

    ; Set interpreter to be "cabal repl"
    ;(setq haskell-process-type 'cabal-repl)

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
                                       (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

                                       ; Set interpreter to be "stack ghci"
                                       (setq haskell-interactive-popup-errors nil)
                                       (setq haskell-process-type 'ghci)
                                       (setq haskell-process-path-ghci "stack")
                                       (setq haskell-process-args-ghci '("ghci"))))

    (eval-after-load 'haskell-mode
      '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
    (eval-after-load 'haskell-cabal
      '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))))



(provide 'grass-haskell)
