(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; Don't autoload packages
(setq package-enable-at-startup nil)

(package-initialize)

;; Detect online status, from ESK
(require 'cl)
(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; ELPA
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents)))

(defvar grass/packages
  '(exec-path-from-shell   ; Use my shell's path

    volatile-highlights    ; Highlight activities in the UI
    solarized-theme

    yasnippet

    helm
    helm-projectile
    helm-ag
    flx-ido
    ido-vertical-mode
    ido-ubiquitous
    popwin
    dired-single

    ag
    magit

    typo

    evil
    evil-surround
    evil-leader
    evil-commentary
    evil-matchit

    ; rainbow-mode
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

    company

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
    hl-sexp
    clj-refactor

    sml-mode
    haskell-mode
    hi2
    ghc
    company-ghc

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



(defun grass/ensure-packages-installed (packages)
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

(grass/ensure-packages-installed grass/packages)

(provide 'grass-packages)
