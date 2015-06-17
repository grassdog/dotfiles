(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; Don't autoload packages
(setq package-enable-at-startup nil)

(package-initialize)

(defvar grass-packages
  '(exec-path-from-shell   ; Use my shell's path

    volatile-highlights    ; Highlight activities in the UI
    solarized-theme

    ethan-wspace

    yasnippet

    helm
    helm-projectile
    helm-ag
    flx-ido
    ; ido-vertical-mode
    ido-ubiquitous
    smex

    ag
    magit

    evil
    evil-surround
    evil-leader
    evil-commentary
    evil-matchit
    ;evil-args
    ;evil-rebellion

    rainbow-mode
    paredit
    rainbow-delimiters

    css-mode
    web-mode
    scss-mode
    slim-mode

    js2-mode
    coffee-mode

    markdown-mode
    haml-mode

    yaml-mode
    feature-mode

    ;; Ruby
    ruby-end
    ruby-block
    ruby-tools
    ;; TODO Get these working
    enh-ruby-mode
    rspec-mode
    chruby

    ;; Clojure
    clojure-mode
    cider

    sml-mode
    haskell-mode
    python
    puppet-mode
    rust-mode

    ;; Wait list

    ; elisp-slime-nav
    ; expand-region
    ; flycheck
    ; wrap-region
    ; multiple-cursors

    )
  "A list of packages to ensure are installed at launch.")



(defun grass-ensure-packages-installed (packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(grass-ensure-packages-installed grass-packages)

(provide 'grass-packages)