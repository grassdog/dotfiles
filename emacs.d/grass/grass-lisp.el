(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode))


(defun grass/set-shift-width ()
   (setq evil-shift-width 2))

(add-hook 'lisp-mode-hook 'grass/set-shift-width)
(add-hook 'emacs-lisp-mode-hook 'grass/set-shift-width)
(add-hook 'clojure-mode-hook 'grass/set-shift-width)
(add-hook 'cider-repl-mode-hook 'grass/set-shift-width)
(add-hook 'scheme-mode-hook 'grass/set-shift-width)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (define-key global-map (kbd "C-c C-e") 'eval-print-last-sexp)))

;; hl-sexp: minor mode to highlight s-expression
(use-package hl-sexp
  :init
  (add-hook 'clojure-mode-hook 'hl-sexp-mode)
  (add-hook 'lisp-mode-hook 'hl-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
  (add-hook 'scheme-mode-hook 'hl-sexp-mode))


(use-package rainbow-delimiters
  :init
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))


(use-package cider
  :config
  ;; REPL history file
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; nice pretty printing
  (setq cider-repl-use-pretty-printing t)

  ;; nicer font lock in REPL
  (setq cider-repl-use-clojure-font-lock t)

  ;; result prefix for the REPL
  (setq cider-repl-result-prefix ";; => ")

  ;; never ending REPL history
  (setq cider-repl-wrap-history t)

  ;; looong history
  (setq cider-repl-history-size 3000)

  ;; error buffer not popping up
  (setq cider-show-error-buffer nil)

  :init

  ;; eldoc for clojure
  (add-hook 'cider-mode-hook #'eldoc-mode)

  ;; company mode for completion
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))


;; clj-refactor and dependencies
(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)

              ;; no auto sort
              (setq cljr-auto-sort-ns nil)

              ;; do not prefer prefixes when using clean-ns
              (setq cljr-favor-prefix-notation nil)
              ;; insert keybinding setup here
              (cljr-add-keybindings-with-prefix "C-c RET"))))

(provide 'grass-lisp)
