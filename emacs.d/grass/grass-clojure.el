(require 'clojure-mode)
(require 'cider)

;; Paredit
(require 'paredit)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)

;; hl-sexp: minor mode to highlight s-expression
(require 'hl-sexp)

(add-hook 'clojure-mode-hook #'hl-sexp-mode)
(add-hook 'lisp-mode-hook #'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)

;; Cider

;; Clojure IDE and REPL for Emacs
(require 'cider)

;; autocompletion
(require 'company)

;; REPL related stuff

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

;; eldoc for clojure
(add-hook 'cider-mode-hook #'eldoc-mode)

;; error buffer not popping up
(setq cider-show-error-buffer nil)

;; company mode for completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; clj-refactor and dependencies
(require 'clj-refactor)

(add-hook 'clojure-mode-hook
  (lambda ()
    (subword-mode +1)
    (clj-refactor-mode 1)
    ;; insert keybinding setup here
    (cljr-add-keybindings-with-prefix "C-c RET")))

(add-hook 'clojure-mode-hook #'yas-minor-mode)

;; no auto sort
(setq cljr-auto-sort-ns nil)

;; do not prefer prefixes when using clean-ns
(setq cljr-favor-prefix-notation nil)

(provide 'grass-clojure)
