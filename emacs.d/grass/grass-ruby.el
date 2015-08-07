
(use-package ruby-tools
  :ensure t
  :diminish ruby-tools-mode)

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
  :init
  (add-hook 'enh-ruby-mode-hook
    (lambda ()
      ;; turn off the annoying input echo in irb
      (setq comint-process-echoes t)
      (ruby-tools-mode +1)
      (setq evil-shift-width ruby-indent-level))))

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

;; Set up hs-mode (HideShow) for Ruby
(add-to-list 'hs-special-modes-alist
             `(enh-ruby-mode
               ,(rx (or "def" "class" "module" "do")) ; Block start
               ,(rx (or "end"))                       ; Block end
               ,(rx (or "#" "=begin"))                ; Comment start
               enh-ruby-forward-sexp nil))

(provide 'grass-ruby)
