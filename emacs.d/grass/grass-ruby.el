
(use-package ruby-end
  :ensure t)

(use-package ruby-tools
  :ensure t)

(use-package enh-ruby-mode
  :ensure t
  :mode ("\\.rb\\'"  "\\.rake\\'" "Rakefile\\'" "\\.gemspec\\'" "\\.ru\\'" "Gemfile\\'" "Guardfile\\'" "Capfile\\'" "\\.thor\\'" "Thorfile\\'" "Vagrantfile\\'")
  :interpreter "ruby"
  :init
  (add-hook 'enh-ruby-mode-hook
    (lambda ()
      ;; turn off the annoying input echo in irb
      (setq comint-process-echoes t)
      (ruby-end-mode +1)
      (ruby-tools-mode +1)
      (setq evil-shift-width ruby-indent-level)
      ;; CamelCase aware editing operations
      (subword-mode +1))))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

(use-package chruby
  :ensure t
  :init
  (chruby))

(use-package rspec-mode
  :ensure t)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(provide 'grass-ruby)
