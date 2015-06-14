(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar grass-packages
  '(
    exec-path-from-shell   ; Use my shell's path
    volatile-highlights    ; Highlight activities in the UI
    solarized-theme
    zenburn-theme
    yasnippet

    ag
    helm
    helm-projectile
    magit

    evil
    evil
    evil-surround
    evil-leader
    ;evil-rebellion

    rainbow-mode
    paredit
    rainbow-delimiters

    coffee-mode
    css-mode
    feature-mode
    web-mode
    haml-mode
    markdown-mode
    scss-mode
    slim-mode
    yaml-mode

    ;; Ruby
    ruby-end
    ruby-block
    ruby-tools
    inf-ruby
    chruby

    ;; Clojure
    clojure-mode
    clojure-test-mode
    nrepl

    sml-mode
    haskell-mode
    python
    puppet-mode
    rust-mode

    ;; Wait list

    ; elisp-slime-nav
    ; expand-region
    ; dash
    ; flycheck
    ; wrap-region
    ; multiple-cursors

    )
  "A list of packages to ensure are installed at launch.")


;; install the missing packages
(defun grass-install-packages ()
  "Ensure all of my required packages are installed"
  (interactive)
  (dolist (package grass-packages)
    (when (not (package-installed-p package))
      (package-install package))))

;; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents)
  (grass-install-packages))

(provide 'grass-packages)
