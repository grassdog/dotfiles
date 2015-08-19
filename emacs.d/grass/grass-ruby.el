
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

  (use-package projectile-rails
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
      ; (flycheck-mode t)
      (setq evil-shift-width ruby-indent-level))))

(use-package chruby
  :config
  (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding))

(provide 'grass-ruby)
