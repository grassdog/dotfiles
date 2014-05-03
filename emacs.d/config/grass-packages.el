(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-user-dir (expand-file-name "elpa" grass-root-dir))
(package-initialize)

(defvar grass-packages
  '(elisp-slime-nav
    exec-path-from-shell
    expand-region
    flycheck
    dash
    gist
    solarized-theme
    volatile-highlights
    yasnippet

    ;; TODO: This guy screws the mini buffer
    wrap-region
    multiple-cursors

    helm
    helm-projectile

    magit
    melpa

    rainbow-mode
    paredit
    rainbow-delimiters

    coffee-mode
    css-mode
    feature-mode

    haml-mode
    haskell-mode
    markdown-mode
    python
    puppet-mode

    ;; Ruby
    ruby-end
    ruby-block
    ruby-tools
    inf-ruby
    yari

    ;; Clojure
    clojure-mode
    clojure-test-mode
    nrepl

    sml-mode

    scss-mode
    slim-mode
    yaml-mode)
  "A list of packages to ensure are installed at launch.")

;; Packages to try
;; ack-and-a-half

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

(require 'dash)

(provide 'grass-packages)
;;; grass-packages ends here
