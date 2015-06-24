(require 'clojure-mode)
(require 'cider)

(eval-after-load 'clojure-mode
  '(progn
     (defun grass/clojure-mode-defaults ()
       (subword-mode +1)
       (paredit-mode +1)
       (rainbow-delimiters-mode +1))

     (setq grass/clojure-mode-hook 'grass/clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'grass/clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun grass/cider-repl-mode-defaults ()
       (subword-mode +1)
       (rainbow-delimeters-mode +1)
       (whitespace-mode -1))

     (setq grass/cider-repl-mode-hook 'grass/cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'grass/cider-repl-mode-hook)))))

(provide 'grass-clojure)
