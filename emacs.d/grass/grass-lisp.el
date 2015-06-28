(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode))


;; Paredit
(use-package paredit
  :ensure t
  :init
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

;; TODO Fix this
;; (defun grass/set-shift-width
;;     (setq evil-shift-width 2))

;; (add-hook 'lisp-mode-hook 'grass/set-shift-width)
;; (add-hook 'emacs-lisp-mode-hook 'grass/set-shift-width)
;; (add-hook 'clojure-mode-hook 'grass/set-shift-width)
;; (add-hook 'cider-repl-mode-hook 'grass/set-shift-width)
;; (add-hook 'scheme-mode-hook 'grass/set-shift-width)

;; hl-sexp: minor mode to highlight s-expression
(use-package hl-sexp
  :ensure t
  :init
  (setq hl-sexp-background-color "#002b36")
  (add-hook 'clojure-mode-hook 'hl-sexp-mode)
  (add-hook 'lisp-mode-hook 'hl-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
  (add-hook 'scheme-mode-hook 'hl-sexp-mode))


(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))


(use-package cider
  :ensure t
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
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (subword-mode +1)
              (clj-refactor-mode 1)

              ;; no auto sort
              (setq cljr-auto-sort-ns nil)

              ;; do not prefer prefixes when using clean-ns
              (setq cljr-favor-prefix-notation nil)
              ;; insert keybinding setup here
              (cljr-add-keybindings-with-prefix "C-c RET"))))

(provide 'grass-lisp)
