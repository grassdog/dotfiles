
(use-package enh-ruby-mode
  :mode (("\\.rb$"        . enh-ruby-mode)
         ("\\.ru$"        . enh-ruby-mode)
         ("\\.rake$"      . enh-ruby-mode)
         ("\\.gemspec$"   . enh-ruby-mode)
         ("\\.?pryrc$"    . enh-ruby-mode)
         ("/Gemfile$"     . enh-ruby-mode)
         ("/Guardfile$"   . enh-ruby-mode)
         ("/Capfile$"     . enh-ruby-mode)
         ("/Vagrantfile$" . enh-ruby-mode)
         ("/Rakefile$"    . enh-ruby-mode))
  :interpreter "ruby"
  :config

  (use-package inf-ruby
    :config
    (setq inf-ruby-default-implementation "pry")
    (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

  (use-package rspec-mode)

  (use-package ruby-end
    :disabled t)

  (use-package projectile-rails
    :disabled t
    :config
    (setq projectile-rails-expand-snippet nil)
    (add-hook 'projectile-mode-hook 'projectile-rails-on))

  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")

  ;; Set up hs-mode (HideShow) for Ruby
  (add-to-list 'hs-special-modes-alist
              `(enh-ruby-mode
                ,(rx (or "def" "class" "module" "do")) ; Block start
                ,(rx (or "end"))                       ; Block end
                ,(rx (or "#" "=begin"))                ; Comment start
                enh-ruby-forward-sexp nil))

  (add-hook 'enh-ruby-mode-hook
    (lambda ()
      ;; turn off the annoying input echo in irb
      (setq comint-process-echoes t)

      ;; Indentation
      (setq ruby-indent-level 2)
      (setq ruby-deep-indent-paren nil)
      (setq enh-ruby-bounce-deep-indent t)
      (setq enh-ruby-hanging-brace-indent-level 2)
      (setq enh-ruby-indent-level 2)
      (setq enh-ruby-deep-indent-paren nil)
      (setq evil-shift-width 2)

      (define-key enh-ruby-mode-map [backtab] 'evil-shift-left-line)

      ;; (flycheck-mode t)
      ;; Abbrev mode seems broken for some reason
      (abbrev-mode -1))))


(use-package chruby
  :config
  (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding))

(provide 'grass-ruby)
