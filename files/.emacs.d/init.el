;; Temporarily up GC limit to speed up start up
(setq gc-cons-threshold 100000000)
(run-with-idle-timer
  5 nil
  (lambda ()
    (setq gc-cons-threshold 1000000)
    (message "gc-cons-threshold restored to %S"
      gc-cons-threshold)))

;;;;;;;;;;;;;;;;;
;; Use Package ;;
;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil) ;; Don't load packages on startup
(setq package-archives '(("org"           . "https://orgmode.org/elpa/")
                          ("gnu"          . "https://elpa.gnu.org/packages/")
                          ("melpa"        . "https://melpa.org/packages/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Debug package loads
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; Give me an imenu of packages in use
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some base libraries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.secrets" t)

(use-package diminish)

; (require 'diminish)
(require 'cl)

;; UTF-8 Thanks
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq org-export-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; File paths
(defvar grass/dotfiles-dir (file-name-directory load-file-name)
  "The root dir of my Emacs config.")
(defvar grass/savefile-dir (expand-file-name "savefile" grass/dotfiles-dir)
  "This folder stores all the automatically generated save/history-files.")

;; Ensure savefile directory exists
(unless (file-exists-p grass/savefile-dir)
  (make-directory grass/savefile-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up load paths
(add-to-list 'load-path (expand-file-name "vendor" grass/dotfiles-dir))

(setq user-full-name "Ray Grasso"
  user-mail-address "ray.grasso@gmail.com")

;; Fix our shell environment on OSX
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :defer 1
    :config
    (exec-path-from-shell-initialize))

  ;; Default font thanks
  (set-frame-font "Operator Mono-13:weight=light"))

;; Some terminal key sequence mapping hackery
(defadvice terminal-init-xterm
  (after map-C-comma-escape-sequence activate)
  (define-key input-decode-map "\e[1;," (kbd "C-,")))

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

(setq grass/leader1 "SPC")
(setq grass/leader2 ",")

;; Faster
(setq font-lock-verbose nil)

;; no jerky scrolling
(setq scroll-conservatively 101)

;; Use right alt for extended character insertion
(setq mac-right-option-modifier nil)

;; Move point to the help window
(setq help-window-select t)

;; Get rid of chrome
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; No blinking cursor
(blink-cursor-mode -1)

;; No startup screen
(setq inhibit-startup-screen t)

;; No bell thanks
(setq ring-bell-function 'ignore)

;; Save clipboard contents into kill-ring before replacing them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences
(setq-default sentence-end-double-space nil)

;; Nice scrolling
(setq scroll-margin 4
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

;; Enable some stuff
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Echo commands quickly
(setq echo-keystrokes 0.02)

;; Slower mouse scroll
(setq mouse-wheel-scroll-amount '(1))

;; A more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
  '("" invocation-name " - " (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                      "%b"))))

;; Follow symlinks by default
(setq vc-follow-symlinks t)

;; Don't combine tag tables thanks
(setq tags-add-tables nil)

;; Automatically load changed tags files
(setq tags-revert-without-query t)

;; Don't pop up new frames on each call to open
(setq ns-pop-up-frames nil)

;; Use system trash
(setq delete-by-moving-to-trash t)

;; Lighter line continuation arrows
(define-fringe-bitmap 'left-curly-arrow [0 64 72 68 126 4 8 0])
(define-fringe-bitmap 'right-curly-arrow [0 2 18 34 126 32 16 0])

;; Wrap lines for text modes
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(diminish 'visual-line-mode "")
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Make files with the same name have unique buffer names
(setq uniquify-buffer-name-style 'forward)

;; Delete selected regions
(delete-selection-mode t)
(transient-mark-mode nil)
(setq select-enable-clipboard nil)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)

;; World times
(setq display-time-world-list '(("Australia/Brisbane" "Brisbane")
                                 ("Australia/Melbourne" "Melbourne")
                                 ("Europe/London" "London")
                                 ("America/New_York" "New York")
                                 ("America/Los_Angeles" "San Francisco")))

;; Base 10 for inserting quoted chars please
(setq read-quoted-char-radix 10)

;; Silence advice warnings
(setq ad-redefinition-action 'accept)


;;;;;;;;;;;;;;;;
;; Encryption ;;
;;;;;;;;;;;;;;;;

(setq epg-gpg-program "gpg2")
(setq epa-file-encrypt-to "ray.grasso@gmail.com")


;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;

;; Must require this before spaceline
(use-package anzu
  :diminish anzu-mode
  :defer 3
  :init (global-anzu-mode +1))

;; Disable themes before loading them (in daemon mode esp.)
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Set default frame size
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 110))

(use-package spacemacs-theme
  :defer t
  :config
  (setq spacemacs-theme-comment-bg nil)
  :init
  (load-theme 'spacemacs-dark t))

(defun grass/set-gui-config ()
  "Enable my GUI settings"
  (interactive)
  (menu-bar-mode +1)
  ;; Highlight the current line
  (global-hl-line-mode +1))

(defun grass/set-terminal-config ()
  "Enable my terminal settings"
  (interactive)
  (xterm-mouse-mode 1)
  (menu-bar-mode -1))

(defun grass/set-ui ()
  (if (display-graphic-p)
    (grass/set-gui-config)
    (grass/set-terminal-config)))

(defun grass/set-frame-config (&optional frame)
  "Establish settings for the current terminal."
  (with-selected-frame frame
    (grass/set-ui)))

;; Only need to set frame config if we are in daemon mode
(if (daemonp)
  (add-hook 'after-make-frame-functions 'grass/set-frame-config)
  ;; Load theme on app creation
  (grass/set-ui))

;;;;;;;;;;;;;;;
;; UI & Help ;;
;;;;;;;;;;;;;;;

(use-package hydra)

;; Keep this for its scoring algorithm
(use-package flx-ido)

(use-package ivy
  :diminish (ivy-mode . "")
  :init
  (use-package ivy-hydra
    :defer 3)

  (setq ivy-height 20)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  ;; Don't count candidates
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
    '((swiper . ivy--regex-plus)
       (t . ivy--regex-fuzzy)))

  (general-define-key :keymaps '(ivy-occur-mode-map ivy-occur-grep-mode-map)
    :states '(normal)
    ",g" 'ivy-occur-revert-buffer)
  (ivy-mode 1))

(use-package counsel
  :init
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (setq counsel-find-file-ignore-regexp
    (concat
      ;; file names beginning with # or .
      "\\(?:\\`[#.]\\)"
      ;; file names ending with # or ~
      "\\|\\(?:[#~]\\'\\)"

      "\\|.*.DS_Store"
      ))
  )

(use-package swiper
  :commands swiper)

(defun swiper-current-word ()
  "Trigger swiper with current word at point"
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (swiper word))))

;; Some swiper bindings
;; "C-c C-o" save output in buffer
;; "M-q" 'swiper-query-replace
;; "C-l" 'swiper-recenter-top-bottom
;;
;; Some ivy occur bindings
;; "C-x C-q" Edit in multi occur mode

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.0)
  (setq which-key-min-display-lines 3)
  (setq which-key-sort-order 'which-key-key-order-alpha)

  (setq which-key-description-replacement-alist
    '(("Prefix Command" . "prefix")
       ("which-key-show-next-page" . "wk next pg")
       ("\\`calc-" . "") ; Hide "calc-" prefixes when listing M-x calc keys
       ("/body\\'" . "") ; Remove display the "/body" portion of hydra fn names
       ("string-inflection" . "si")
       ("counsel-" . "c/")
       ("crux-" . "cx/")
       ("grass/" . "g/")
       ("\\`hydra-" . "+h/")
       ("\\`org-babel-" . "ob/")))
  (which-key-mode 1))

(use-package general
  :init
  (general-evil-setup t))

(use-package browse-kill-ring
  :commands browse-kill-ring)


;; Subtle highlighting of matching parens (global-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (show-paren-mode +1)
                            (setq show-paren-style 'parenthesis)))

;; UI highlight search and other actions
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :defer 3
  :config
  (volatile-highlights-mode t))

;; Text zoom
(defhydra hydra-zoom-text ()
  "zoom text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

;; Simple indenting
(require 'stupid-indent-mode)
(diminish 'stupid-indent-mode "ⓘ")
(add-hook 'rjsx-mode-hook 'stupid-indent-mode)

(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :config
  (progn
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-character ?\|)))


(use-package window-numbering
  :config
  (progn
    (defun window-numbering-install-mode-line (&optional position)
      "Do nothing, the display is handled by the powerline.")
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    (general-define-key
      :states '(normal visual insert emacs)
      :prefix grass/leader1
      :non-normal-prefix "M-SPC"
      "0" 'select-window-0
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "4" 'select-window-4
      "5" 'select-window-5
      "6" 'select-window-6
      "7" 'select-window-7
      "8" 'select-window-8
      "9" 'select-window-9)
    (window-numbering-mode 1)))


;;;;;;;;;;;;;;;;;;;
;; Key Frequency ;;
;;;;;;;;;;;;;;;;;;;

(use-package keyfreq
  :init
  (setq keyfreq-excluded-commands
    '(self-insert-command
       abort-recursive-edit
       forward-char
       backward-char
       previous-line
       next-line
       right-char
       left-char))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and editing history ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      (list (cons ".*" (expand-file-name "~/.emacs-backups/"))))

(setq auto-save-file-name-transforms
  `((".*" "~/.cache/emacs/saves/" t)))

(use-package saveplace
  :config
  ;; Saveplace remembers your location in a file when saving files
  (setq save-place-file (expand-file-name "saveplace" grass/savefile-dir))
  :init
  (save-place-mode 1))

;; Save minibuffer history etc
(use-package savehist
  :defer 2
  :config
  (setq savehist-additional-variables
    ;; search entries
    '(search ring regexp-search-ring)
    ;; save every minute
    savehist-autosave-interval 60
    ;; keep the home clean
    savehist-file (expand-file-name "savehist" grass/savefile-dir))
  (savehist-mode 1))

(use-package recentf
  :defer 2
  :commands recentf-mode
  :config
  (add-to-list 'recentf-exclude "\\ido.hist\\'")
  (add-to-list 'recentf-exclude "/TAGS")
  (add-to-list 'recentf-exclude "/.autosaves/")
  (add-to-list 'recentf-exclude "intero-script")
  (add-to-list 'recentf-exclude "emacs.d/elpa/")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (setq recentf-save-file (expand-file-name "recentf" grass/savefile-dir))
  (setq recentf-max-saved-items 100))

(add-hook 'find-file-hook (lambda () (unless recentf-mode
                                       (recentf-mode)
                                       (recentf-track-opened-file))))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :init
  (global-undo-tree-mode))

(use-package goto-chg
  :commands (goto-last-change goto-last-change-reverse))

(defhydra hydra-goto-history ()
  "change history"
  ("p" goto-last-change "previous")
  ("n" goto-last-change-reverse "next")
  ("g" git-timemachine "git timemachine")
  ("v" undo-tree-visualize "visualise" :exit t)
  ("q" nil "quit"))

(use-package ws-butler
  :diminish ws-butler-mode
  :init
  (setq ws-butler-keep-whitespace-before-point t)
  (ws-butler-global-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil (Trojan horse maneuver) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :preface
  (setq evil-search-module 'evil-search)
  (setq evil-want-integration nil)

  :init

  ;; Evil plugins
  (use-package evil-commentary
    :diminish evil-commentary-mode
    :init
    (evil-commentary-mode))

  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1))

  (use-package evil-anzu
    :init
    (setq anzu-cons-mode-line-p nil))

  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))

  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search-forward
                evil-visualstar/begin-search-backward)
    :init
    (progn
      (define-key evil-visual-state-map (kbd "*")
        'evil-visualstar/begin-search-forward)
      (define-key evil-visual-state-map (kbd "#")
        'evil-visualstar/begin-search-backward)))

  ;; Hopefully this stops Emacs choking on large files
  (setq evil-ex-search-highlight-all nil)

  (use-package evil-search-highlight-persist
    :init
    (setq evil-search-highlight-string-min-len 3)
    (global-evil-search-highlight-persist t)

    (defun grass/remove-search-highlights ()
      "Remove all highlighted search terms."
      (interactive)
      (lazy-highlight-cleanup)
      (evil-search-highlight-persist-remove-all)
      (evil-ex-nohighlight)))

  ;; Cursors
  (defvar dotspacemacs-colorize-cursor-according-to-state t
    "If non nil the cursor color matches the state color in GUI Emacs.")

  (defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                    ("insert" "chartreuse3" (bar . 2))
                                    ("emacs" "SkyBlue2" box)
                                    ("hybrid" "SkyBlue2" (bar . 2))
                                    ("replace" "chocolate" (hbar . 2))
                                    ("evilified" "LightGoldenrod3" box)
                                    ("visual" "gray" (hbar . 2))
                                    ("motion" "plum3" box)
                                    ("lisp" "HotPink1" box)
                                    ("iedit" "firebrick1" box)
                                    ("iedit-insert" "firebrick1" (bar . 2)))
    "Colors assigned to evil states with cursor definitions.")

  (loop for (state color cursor) in spacemacs-evil-cursors
    do
    (eval `(defface ,(intern (format "spacemacs-%s-face" state))
             `((t (:background ,color
                    :foreground ,(face-background 'mode-line)
                    :box ,(face-attribute 'mode-line :box)
                    :inherit 'mode-line)))
             (format "%s state face." state)
             :group 'spacemacs))
    (eval `(setq ,(intern (format "evil-%s-state-cursor" state))
             (list (when dotspacemacs-colorize-cursor-according-to-state color)
               cursor))))

  ;; put back refresh of the cursor on post-command-hook see status of:
  ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
  ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

  (defun spacemacs/state-color-face (state)
    "Return the symbol of the face for the given STATE."
    (intern (format "spacemacs-%s-face" (symbol-name state))))

  (defun spacemacs/state-color (state)
    "Return the color string associated to STATE."
    (face-background (spacemacs/state-color-face state)))

  (defun spacemacs/current-state-color ()
    "Return the color string associated to the current state."
    (face-background (spacemacs/state-color-face evil-state)))

  (defun spacemacs/state-face (state)
    "Return the face associated to the STATE."
    (spacemacs/state-color-face state))

  (defun spacemacs/current-state-face ()
    "Return the face associated to the current state."
    (let ((state (if (eq evil-state 'operator)
                   evil-previous-state
                   evil-state)))
      (spacemacs/state-color-face state)))

  (defun evil-insert-state-cursor-hide ()
    (setq evil-insert-state-cursor '((hbar . 0))))

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (setq-default evil-shift-width 2)
  (setq evil-want-fine-undo t)

  ;; Little word
  (require 'evil-little-word)
  (define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
  (define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
  (define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
  (define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
  (define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
  (define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word)

  (use-package evil-args
    :init
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
    (add-hook 'clojure-mode-hook
      (lambda()
        (setq evil-args-delimiters '(" "))))
    (add-hook 'emacs-lisp-mode-hook
      (lambda()
        (setq evil-args-delimiters '(" ")))))

  (use-package evil-indent-plus
    :init
    (evil-indent-plus-default-bindings))

  ;; Function motion
  (setq evil-move-defun-alist
    '((ruby-mode . (ruby-beginning-of-defun . ruby-end-of-defun))
       (c-mode    . (c-beginning-of-defun . c-end-of-defun))
       (js2-mode  . (js2-beginning-of-defun . js2-end-of-defun))))

  (defun evil-backward-defun (&optional count)
    "Move backward by defun"
    (let* ((count (or count 1))
           (mode-defuns (cdr-safe (assq major-mode evil-move-defun-alist)))
           (begin-defun (or (car-safe mode-defuns) 'beginning-of-defun)))
      (evil-motion-loop (var count)
        (funcall begin-defun))))

  (defun evil-forward-defun (&optional count)
    "Move forward by defun"
    (let* ((count (or count 1))
           (mode-defuns (cdr-safe (assq major-mode evil-move-defun-alist)))
           (end-defun (or (cdr-safe mode-defuns) 'end-of-defun)))
      (evil-motion-loop (var count)
        (funcall end-defun))))

  (evil-define-motion evil-backward-defun-motion (count)
    "Move the cursor to the beginning of the COUNT-th next defun."
    :type exclusive
    (evil-backward-defun count))

  (evil-define-motion evil-forward-defun-motion (count)
    "Move the cursor to the beginning of the COUNT-th next defun."
    :type exclusive
    (evil-forward-defun count))

  (define-key evil-motion-state-map (kbd "glf") 'evil-forward-defun-motion)
  (define-key evil-motion-state-map (kbd "glF") 'evil-backward-defun-motion)

  (evil-mode t)

  ;; Yank till end of line
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
  ;; Easy start and end of line
  (define-key evil-normal-state-map (kbd "H") 'crux-move-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "H") 'crux-move-beginning-of-line)
  (define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Make esc quit everywhere
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key isearch-mode-map [escape] 'isearch-abort)
  (global-set-key [escape] 'keyboard-escape-quit)

  ;; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">>") 'grass/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<<") 'grass/evil-shift-left-visual)
  (define-key evil-visual-state-map (kbd "<S-down>") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "<S-up>") 'evil-previous-visual-line)

  (defun grass/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun grass/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)

  ;; Keep some Emacs stuff
  (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-insert-state-map "\C-a" 'crux-move-beginning-of-line)
  (define-key evil-normal-state-map "\C-a" 'crux-move-beginning-of-line)
  (define-key evil-visual-state-map "\C-a" 'crux-move-beginning-of-line)
  (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
  (define-key evil-visual-state-map "Q" 'call-last-kbd-macro))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments and filling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 80 char wide paragraphs please
(setq-default fill-column 80)

;; Autofill where possible but only in comments when coding
;; http://stackoverflow.com/questions/4477357/how-to-turn-on-emacs-auto-fill-mode-only-for-code-comments
(setq comment-auto-fill-only-comments t)
;; (auto-fill-mode 1)

;; From http://mbork.pl/2015-11-14_A_simple_unfilling_function
(defun grass/unfill-region (begin end)
  "Change isolated newlines in region into spaces."
  (interactive (if (use-region-p)
                 (list (region-beginning)
                   (region-end))
                 (list nil nil)))
  (save-restriction
    (narrow-to-region (or begin (point-min))
      (or end (point-max)))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (if (eq (char-after) ?\n)
        (skip-chars-forward "\n")
        (delete-char -1)
        (insert ?\s)))))

;; TODO Remove region params to interactive
;; http://stackoverflow.com/a/21051395/62023
(defun grass/comment-box (beg end &optional arg)
  (interactive "*r\np")
  ;; (when (not (region-active-p))
  (when (not (and transient-mark-mode mark-active))
    (setq beg (point-at-bol))
    (setq end (point-at-eol)))
  (let ((fill-column (- fill-column 6)))
    (fill-region beg end))
  (comment-box beg end arg)
  (grass/move-point-forward-out-of-comment))

(defun grass/point-is-in-comment-p ()
  "t if point is in comment or at the beginning of a commented line, otherwise nil"
  (or (nth 4 (syntax-ppss))
    (looking-at "^\\s *\\s<")))

(defun grass/move-point-forward-out-of-comment ()
  "Move point forward until it's no longer in a comment"
  (while (grass/point-is-in-comment-p)
    (forward-char)))

;; Comment annotations
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
    nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\|DEBUG\\|GRASS\\)"
            1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulating Text ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init
  (setq drag-stuff-except-modes '(org-mode))
  (setq drag-stuff-modifier '(meta super))
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))


;; Keep system clipboard separate from kill ring
(use-package simpleclip
  :defer 2
  :config
  (simpleclip-mode 1))

(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html))

(use-package string-inflection
  :commands (string-inflection-underscore
              string-inflection-upcase
              string-inflection-lower-camelcase
              string-inflection-camelcase
              string-inflection-lisp))

(defhydra hydra-change-case ()
  "toggle word case"
  ("c" capitalize-word "Capitalize")
  ("u" upcase-word "UPPER")
  ("l" downcase-word "lower")
  ("s" string-inflection-underscore "lower_snake")
  ("n" string-inflection-upcase "UPPER_SNAKE")
  ("a" string-inflection-lower-camelcase "lowerCamel")
  ("m" string-inflection-camelcase "UpperCamel")
  ("d" string-inflection-lisp "dash-case"))

;; Better zap to char
(use-package zop-to-char
  :commands (zop-to-char zop-up-to-char))

(global-set-key [remap zap-to-char] 'zop-to-char)

;; C-' in iedit mode makes it look like swiper
(use-package iedit
  :commands 'iedit-mode
  :defines grass/iedit-dwim
  :config
  (setq iedit-current-symbol-default t
    iedit-only-at-symbol-boundaries t)
  (defun grass/iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
      (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
            (iedit-done)
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max))))))))

(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                           (and (looking-back ">" 1) (looking-at "<"))
                           (and (looking-back "(" 1) (looking-at ")"))
                           (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))


;;;;;;;;;;;;;;;;;;;;;
;; Window handling ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :commands (ace-window))
(winner-mode 1)

(use-package windmove
  :commands
  (windmove-left windmove-down windmove-up windmove-right))

(defun grass/move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
    (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun grass/move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
    (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun grass/move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
    (enlarge-window arg)
    (shrink-window arg)))

(defun grass/move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
    (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-buffer (:color blue :columns 3)
  "
                Buffers :
  "
  ("n" grass/next-useful-buffer "next useful" :color red)
  ("N" next-buffer "next" :color red)
  ("b" ivy-switch-buffer "switch")
  ("B" ibuffer "ibuffer")
  ("p" grass/previous-useful-buffer "prev useful" :color red)
  ("P" previous-buffer "prev" :color red)
  ("C-b" buffer-menu "buffer menu")
  ("+" evil-buffer-new "new")
  ("d" kill-this-buffer "delete" :color red)
  ;; don't come back to previous buffer after delete
  ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
  ("s" save-buffer "save" :color red))

(defun grass/window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa."
  (interactive)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defhydra hydra-window ()
  "
Movement^^        ^Split^          ^Switch^        ^Resize^
----------------------------------------------------------------
_h_ ←            _v_ertical        _b_uffer        _q_ X←
_j_ ↓            _x_ horizontal    _f_ind files    _w_ X↓
_k_ ↑            _z_ undo          _a_ce 1         _e_ X↑
_l_ →            _Z_ reset         _s_wap          _r_ X→
_F_ollow         _D_lt Other       _S_ave          max_i_mize
_SPC_ cancel     _o_nly this       _d_elete
                               _t_oggle split
"
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("q" grass/move-splitter-left nil)
  ("w" grass/move-splitter-down nil)
  ("e" grass/move-splitter-up nil)
  ("r" grass/move-splitter-right nil)
  ("b" ivy-switch-buffer nil)
  ("f" counsel-find-file nil)
  ("F" follow-mode nil)
  ("t" grass/window-toggle-split-direction nil)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
           'hydra-window/body))
    nil)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
    nil)
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
    nil)
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
           'hydra-window/body)) nil)
  ("S" save-buffer nil)
  ("d" delete-window nil)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
           'hydra-window/body))
    nil)
  ("o" delete-other-windows nil)
  ("i" ace-maximize-window nil)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
    nil)
  ("Z" winner-redo nil)
  ("SPC" nil nil))


(use-package origami
  :commands (origami-toggle-node
              origami-show-only-node
              origami-show-all-nodes
              origami-undo
              origami-redo
              origami-recursively-toggle-node)
  :config
  (add-hook 'prog-mode-hook
    (lambda ()
      (origami-mode 1))))


;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

;; http://stackoverflow.com/a/8257269/62023
(defun grass/minibuffer-insert-word-at-point ()
  "Get word at point in original buffer and insert it to minibuffer."
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (insert word))))

(defun grass/minibuffer-setup-hook ()
  (local-set-key (kbd "C-w") 'grass/minibuffer-insert-word-at-point))

(add-hook 'minibuffer-setup-hook 'grass/minibuffer-setup-hook)

(defhydra hydra-insert-timestamp (:color blue :hint nil)
  "
Timestamps: (_q_uit)
  Date: _I_SO, _U_S, US With _Y_ear and _D_ashes, US In _W_ords
   Date/Time: _N_o Colons or _w_ith
    Org-Mode: _R_ight Now or _c_hoose
"
  ("q" nil)

  ("I" help/insert-datestamp)
  ("U" help/insert-datestamp-us)
  ("Y" help/insert-datestamp-us-full-year)
  ("D" help/insert-datestamp-us-full-year-and-dashes)
  ("W" help/insert-datestamp-us-words)

  ("N" help/insert-timestamp-no-colons)
  ("w" help/insert-timestamp)

  ("R" help/org-time-stamp-with-seconds-now)
  ("c" org-time-stamp))
(global-set-key (kbd "C-t") #'help/hydra/timestamp/body)
(defun help/insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%F")))
(defun help/insert-datestamp-us ()
  "Produces and inserts a US datestamp."
  (interactive)
  (insert (format-time-string "%m/%d/%y")))
(defun help/insert-datestamp-us-full-year-and-dashes ()
  "Produces and inserts a US datestamp with full year and dashes."
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))
(defun help/insert-datestamp-us-full-year ()
  "Produces and inserts a US datestamp with full year."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))
(defun help/insert-datestamp-us-words ()
  "Produces and inserts a US datestamp using words."
  (interactive)
  (insert (format-time-string "%A %B %d, %Y")))
(defun help/insert-timestamp-no-colons ()
  "Inserts a full ISO 8601 format timestamp with colons replaced by hyphens."
  (interactive)
  (insert (help/get-timestamp-no-colons)))
(defun help/insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%F")))
(defun help/get-timestamp-no-colons ()
  "Produces a full ISO 8601 format timestamp with colons replaced by hyphens."
  (interactive)
  (let* ((timestamp (help/get-timestamp))
         (timestamp-no-colons (replace-regexp-in-string ":" "-" timestamp)))
    timestamp-no-colons))
(defun help/get-timestamp ()
  "Produces a full ISO 8601 format timestamp."
  (interactive)
  (let* ((timestamp-without-timezone (format-time-string "%Y-%m-%dT%T"))
         (timezone-name-in-numeric-form (format-time-string "%z"))
         (timezone-utf-offset
          (concat (substring timezone-name-in-numeric-form 0 3)
                  ":"
                  (substring timezone-name-in-numeric-form 3 5)))
         (timestamp (concat timestamp-without-timezone
                            timezone-utf-offset)))
    timestamp))
(defun help/insert-timestamp ()
  "Inserts a full ISO 8601 format timestamp."
  (interactive)
  (insert (help/get-timestamp)))
(defun help/org-time-stamp-with-seconds-now ()
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-time-stamp)))

(defun grass/today ()
  (format-time-string "%Y.%m.%d - %a"))

(defun grass/insert-datetime (arg)
  "Insert ISO8601 date with time"
  (interactive "P")
  (insert (format-time-string "%FT%T%z")))

(defun grass/insert-date (arg)
  "Insert date"
  (interactive "P")
  (insert (format-time-string "%F")))

(defun grass/insert-today ()
  "Insert date with day of the week"
  (interactive)
  (insert (grass/today)))

(defun grass/insert-org-date-header ()
  (interactive)
  (insert (concat "* " (grass/today))))

(defun grass/view-url-in-buffer ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
          (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
      ((search-forward "<html" nil t) (html-mode)))))

(defun grass/copy-buffer-filename ()
  "Copy filename of buffer into system clipboard."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
      (message (simpleclip-set-contents file-name))
      (error "Buffer not visiting a file"))))

(defun grass/indent-buffer ()
  "Indents the entire buffer."
  (indent-region (point-min) (point-max)))

(defun grass/indent-region-or-buffer ()
  "Indents a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (message "Indented selected region."))
      (progn
        (grass/indent-buffer)
        (message "Indented buffer.")))))

(defun grass/open-this-file-as-other-user (user)
  "Edit current file as USER, using `tramp' and `sudo'.  If the current
buffer is not visiting a file, prompt for a file name."
  (interactive "sEdit as user (default: root): ")
  (when (string= "" user)
    (setq user "root"))
  (let* ((filename (or buffer-file-name
                       (read-file-name (format "Find file (as %s): "
                                               user))))
         (tramp-path (concat (format "/sudo:%s@localhost:" user) filename)))
    (if buffer-file-name
        (find-alternate-file tramp-path)
      (find-file tramp-path))))

;;
;; Buffer switching
;;

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun grass/useful-buffer-p (&optional potential-buffer-name)
  "Return t if current buffer is a user buffer, else nil."
  (interactive)
  (let ((buffer-to-test (or potential-buffer-name (buffer-name))))
    (if (string-equal "*" (substring (s-trim-left buffer-to-test) 0 1))
      nil
      (if (string-match "dired" (symbol-name
                                  (with-current-buffer potential-buffer-name
                                    major-mode)))
        nil
        t))))

(defun grass/next-useful-buffer ()
  "Switch to the next user buffer."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (grass/useful-buffer-p))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun grass/previous-useful-buffer ()
  "Switch to the previous user buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (grass/useful-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun grass/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (let* ((candidate-buffers (remove-if-not
                              #'grass/useful-buffer-p
                              (mapcar (function buffer-name) (buffer-list))))
         (candidate-buffer (nth 1 candidate-buffers)))
         (if candidate-buffer
           (switch-to-buffer (nth 1 candidate-buffers)))))

(defun grass/rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
      (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
                (error "A buffer named '%s' already exists!" new-name))
          (t
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun grass/what-face (pos)
  "Identify the face under point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun grass/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(use-package crux
  :commands (crux-delete-file-and-buffer
              crux-duplicate-current-line-or-region
              crux-kill-other-buffers
              crux-indent-defun
              crux-cleanup-buffer-or-region
              crux-move-beginning-of-line
              crux-transpose-windows
              crux-view-url
              )
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))


(use-package reveal-in-osx-finder
  :commands reveal-in-osx-in-finder)

;;;;;;;;;;;;;;;;;;
;; Common Files ;;
;;;;;;;;;;;;;;;;;;

(defun grass/open-init ()
  "Open Worklog file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun grass/open-work-log ()
  "Open Worklog file"
  (interactive)
  (find-file "~/Dropbox/Notes/Journals/Work.org"))

(defun grass/open-personal-log ()
  "Open Worklog file"
  (interactive)
  (find-file "~/Dropbox/Notes/Journals/Personal.org"))

(defun grass/find-notes ()
  "Find a note in Dropbox/Notes directory"
  (interactive)
  (counsel-file-jump "" (expand-file-name "~/Dropbox/Notes")))

(defun grass/find-tab ()
  "Find tablature file in Dropbox directory"
  (interactive)
  (counsel-file-jump "" (expand-file-name "~/Dropbox/Library/Guitar/Tablature")))

;;;;;;;;;
;; Git ;;
;;;;;;;;;

(use-package magit
  :commands magit-status
  :config
  (use-package evil-magit)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package magithub
  :disabled t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package git-link
  :commands (git-link git-link-commit)
  :config
  (setq git-link-open-in-browser t))

(use-package github-browse-file
  :commands github-browse-file)

(defun grass/open-pull-request ()
  (interactive)
  (browse-url
  (format "https://github.com/%s/pull/new/%s"
    (replace-regexp-in-string
      ".+github\\.com[:/]\\(.+\\)\\(\\.git\\)?" "\\1"
      (magit-get "remote" "origin" "url"))
    (magit-get-current-branch))))

(use-package gist
  :commands
  (gist-buffer gist-buffer-private gist-list gist-region gist-region-private)
  :config
  (progn
    (general-define-key :keymaps '(gist-list-menu-mode-map gist-list-mode-map)
      :states '(normal)
      "RET" 'gist-fetch-current
      ",g" 'gist-list-reload
      ",e" 'gist-edit-current-description
      ",k" 'gist-kill-current
      ",+" 'gist-add-buffer
      ",-" 'gist-remove-file
      ",y" 'gist-print-current-url
      ",b" 'gist-browse-current-url
      ",*" 'gist-star
      ",^" 'gist-unstar
      ",f" 'gist-fork
      ",f" 'gist-fetch-current
      ",K" 'gist-kill-current
      ",o" 'gist-browse-current-url)))


(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle))


(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (setq git-gutter-fr:side 'right-fringe)
  ;; custom graphics that works nice with half-width fringes
  (fringe-helper-define 'git-gutter-fr:added nil
    "..X...."
    "..X...."
    "XXXXX.."
    "..X...."
    "..X...."
    )
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "......."
    "......."
    "XXXXX.."
    "......."
    "......."
    )
  (fringe-helper-define 'git-gutter-fr:modified nil
    "..X...."
    ".XXX..."
    "XX.XX.."
    ".XXX..."
    "..X...."
    )
  :init
  (global-git-gutter-mode t))

(use-package ediff
  :init
  (progn
    (setq-default
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-horizontally)

    ;; Restore window layout when done
    (add-hook 'ediff-quit-hook #'winner-undo)))


;;;;;;;;;;;;;;;;;;;;;;
;; Symbol insertion ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package char-menu
  :commands char-menu
  ;; Em-dash is first
  :config (setq char-menu '("—" "‘’" "“”" "…" "«»" "–"
                             ("Typography" "⋅" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                             ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
                             ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto save on focus lost ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grass/toggle-auto-save ()
  "Toggle auto save setting"
  (interactive)
  (setq auto-save-default (if auto-save-default nil t)))

(defun grass/auto-save-all()
  "Save all modified buffers that point to files."
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
        (basic-save-buffer)))))

(add-hook 'auto-save-hook 'grass/auto-save-all)
(add-hook 'mouse-leave-buffer-hook 'grass/auto-save-all)
(add-hook 'focus-out-hook 'grass/auto-save-all)


;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(defun grass/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."

  (setq dired-use-ls-dired nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-omit-verbose nil)
  (setq dired-omit-files
    (rx (or (seq bol (? ".") "#")       ;; emacs autosave files
          (seq "~" eol)                 ;; backup-files
          (seq bol "CVS" eol)           ;; CVS dirs
          (seq ".pyc" eol)
          (seq bol ".DS_Store" eol)
          (seq bol ".tern-port" eol))))

  (general-define-key :keymaps 'dired-mode-map
    :states '(normal visual insert emacs)
    :prefix grass/leader2
    :non-normal-prefix "M-,"
    "g" 'revert-buffer)

  (define-key dired-mode-map [return] 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")
    (function
      (lambda nil (interactive) (dired-jump))))
  (define-key dired-mode-map (kbd "-")
    (function
      (lambda nil (interactive) (dired-jump)))))

(if (boundp 'dired-mode-map)
  (grass/dired-init)
  (add-hook 'dired-load-hook 'grass/dired-init))

(add-hook 'dired-mode-hook
  (lambda ()
    (put 'dired-find-alternate-file 'disabled nil)
    (dired-omit-mode t)
    (dired-hide-details-mode t)))

(eval-after-load "dired"
  '(progn
     (general-define-key :keymaps 'dired-mode-map
       :states '(normal visual insert emacs)
       :prefix grass/leader1
       :non-normal-prefix "M-SPC"

       ;; Remove dired mapping on space to get back my leader
       " " nil)))

(use-package dired+
  :commands (dired-omit-mode dired-jump)
  :config
  ;; Chill the colours in dired
  (setq font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
  (diminish 'dired-omit-mode ""))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and Replace ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; More standard regex
(use-package pcre2el
  :diminish (pcre-mode . "	ⓧ")
  :init
  (pcre-mode))

;; http://sachachua.com/blog/2008/07/emacs-keyboard-shortcuts-for-navigating-code/
(defun grass/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
      (lambda ()
        (skip-syntax-forward "w_")
        (point)))))

(define-key isearch-mode-map (kbd "C-x") 'grass/isearch-yank-current-word)

(defun grass/replace-regexp-in-entire-buffer ()
  "Perform regular-expression replacement throughout buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
      (call-interactively 'anzu-replace-regexp)))

(defun grass/query-replace-regexp-in-entire-buffer ()
  "Perform regular-expression replacement throughout buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
      (call-interactively 'anzu-query-replace-regexp)))

(use-package ag
  :commands (ag ag-project))

(use-package ripgrep
  :commands (ripgrep-regexp))

(use-package dumb-jump
  :commands (dumb-jump-go dumb-jump-back dumb-jump-quick-look dump-jump-go-other-window dump-jump-go-prompt dumb-jump-go-prefer-external)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-selector 'ivy))


;;;;;;;;;;;;;;;
;; Selection ;;
;;;;;;;;;;;;;;;

(use-package expand-region
  :commands er/expand-region
  :config
  (setq expand-region-contract-fast-key "V"
    expand-region-reset-fast-key "r"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete and snippets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; abbrev-mode for common typos
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(diminish 'abbrev-mode " ⓐ")
(setq-default abbrev-mode t)

(use-package company
  :diminish (company-mode . " ⓒ")
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-global-modes
    '(not markdown-mode org-mode erc-mode))

  ;; Tweak fonts
  (custom-set-faces
    '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
    '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(eval-after-load 'company
  '(progn
     (general-define-key :keymaps 'company-active-map
       "TAB" 'nil
       "<tab>" 'nil
       "RET" 'company-complete-selection
       "S-TAB" 'company-select-previous
       "<backtab>" 'company-select-previous
       "ESC" 'company-abort)))

(add-hook 'after-init-hook 'global-company-mode)

;; replace dabbrev-expand with Hippie expand
(setq hippie-expand-try-functions-list
  '(
     ;; Try to expand yasnippet snippets based on prefix
     yas-hippie-try-expand

     ;; Try to expand word "dynamically", searching the current buffer.
     try-expand-dabbrev
     ;; Try to expand word "dynamically", searching all other buffers.
     try-expand-dabbrev-all-buffers
     ;; Try to expand word "dynamically", searching the kill ring.
     try-expand-dabbrev-from-kill
     ;; Try to complete text as a file name, as many characters as unique.
     try-complete-file-name-partially
     ;; Try to complete text as a file name.
     try-complete-file-name
     ;; Try to expand word before point according to all abbrev tables.
     try-expand-all-abbrevs
     ;; Try to complete the current line to an entire line in the buffer.
     try-expand-list
     ;; Try to complete the current line to an entire line in the buffer.
     try-expand-line
     ;; Try to complete as an Emacs Lisp symbol, as many characters as
     ;; unique.
     try-complete-lisp-symbol-partially
     ;; Try to complete word as an Emacs Lisp symbol.
     try-complete-lisp-symbol))

;;;;;;;;;;;;;;
;; Snippets ;;
;;;;;;;;;;;;;;

(use-package yasnippet
  :diminish (yas-minor-mode . " ⓨ")
  :defer 1
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-verbosity 1)
  (yas-global-mode 1))


;;;;;;;;;;;;
;; Parens ;;
;;;;;;;;;;;;

(use-package corral
  :commands (corral-parentheses-backward
              corral-parentheses-forward
              corral-brackets-backward
              corral-brackets-forward
              corral-braces-backward
              corral-braces-forward
              corral-single-quotes-backward
              corral-double-quotes-backward))

(defhydra hydra-surround (:columns 4)
  "Corral"
  ("(" corral-parentheses-backward "Back")
  (")" corral-parentheses-forward "Forward")
  ("[" corral-brackets-backward "Back")
  ("]" corral-brackets-forward "Forward")
  ("{" corral-braces-backward "Back")
  ("}" corral-braces-forward "Forward")
  ("\"" corral-double-quotes-backward "Back")
  ("'" corral-single-quotes-backward "Back")
  ("." hydra-repeat "Repeat"))


;;;;;;;;;;;;;;;
;; Alignment ;;
;;;;;;;;;;;;;;;

;; Modified function from http://emacswiki.org/emacs/AlignCommands
(defun align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                           (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
         (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
    '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
        (repeat . t)
        (group 1 2)
        (spacing 1 1)
        (justify nil t)))
    nil))

(defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (align-repeat start end ,regexp ,justify-right after)))))

(create-align-repeat-x "comma" "," nil t)
(create-align-repeat-x "semicolon" ";" nil t)
(create-align-repeat-x "colon" ":" nil t)
(create-align-repeat-x "equal" "=")
(create-align-repeat-x "hash" "=>")
(create-align-repeat-x "math-oper" "[+\\-*/]")
(create-align-repeat-x "ampersand" "&")
(create-align-repeat-x "bar" "|")
(create-align-repeat-x "left-paren" "(")
(create-align-repeat-x "right-paren" ")" t)


;;;;;;;;;;;;;;;
;; Prog mode ;;
;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

;; Line numbers for coding please
(add-hook 'prog-mode-hook
  (lambda ()
    ;; Treat underscore as a word character
    (modify-syntax-entry ?_ "w")
    (display-line-numbers-mode)
    (rainbow-delimiters-mode)))


;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

;; Fancy tabbing. Set to nil to stop tab indenting a line
(setq-default tab-always-indent t)

(defun grass/toggle-always-indent ()
  "Toggle tab-always-indent setting"
  (interactive)
  (setq tab-always-indent (if tab-always-indent nil t)))

;; Don't use tabs to indent
(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq lisp-indent-offset 2)
(setq-default js2-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default js-indent-level 2)
(setq-default js2-indent-switch-body t)
(setq css-indent-offset 2)
(setq coffee-tab-width 2)
(setq-default py-indent-offset 2)
(setq-default nxml-child-indent 2)
(setq typescript-indent-level 2)
(setq ruby-indent-level 2)
(setq sgml-basic-offset 2)

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

;;;;;;;;;;;;;;;;
;; Whitespace ;;
;;;;;;;;;;;;;;;;

(require 'whitespace)
(diminish 'global-whitespace-mode)
;; Only show bad whitespace (Ignore empty lines at start and end of buffer)
(setq whitespace-style '(face tabs trailing space-before-tab indentation space-after-tab))
(global-whitespace-mode t)

(setq require-final-newline t)



;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(use-package projectile
  :diminish (projectile-mode . " ⓟ")
  :commands (projectile-mode projectile-project-root projectile-ag)
  :defines grass/counsel-ag-current-project
  :config
  (setq projectile-tags-command "rtags -R -e")
  (setq projectile-enable-caching nil)
  (setq projectile-completion-system 'ivy)
  ;; Show unadded files also
  (setq projectile-hg-command "( hg locate -0 -I . ; hg st -u -n -0 )")

  (add-to-list 'projectile-globally-ignored-directories "gems")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "bower_components")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "/emacs.d/elpa/")
  (add-to-list 'projectile-globally-ignored-directories "vendor/cache/")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files ".keep")
  (add-to-list 'projectile-globally-ignored-files "TAGS"))

(use-package counsel-projectile
  :init
  (progn
    (setq projectile-switch-project-action 'counsel-projectile-find-file)

    (general-define-key
      :states '(normal visual insert emacs)
      :prefix grass/leader1
      :non-normal-prefix "M-SPC"

      "P" '(:ignore t :which-key "Projectile")
      "P SPC" 'counsel-projectile
      "Pb"    'counsel-projectile-switch-to-buffer
      "bp"    'counsel-projectile-switch-to-buffer
      "Pd"    'counsel-projectile-find-dir
      "PP"    '(:keymap projectile-command-map :package projectile :which-key "projectile")
      "Ps"    'counsel-projectile-switch-project
      "p"     'counsel-projectile-find-file
      "fp"    'counsel-projectile-find-file)

    (defun grass/counsel-rg-current-project ()
      "Search in current project with `rg'."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
          (counsel-rg "" dir)
          (message "error: Not in a project."))))
    :init
    (projectile-global-mode t)))


;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :defer t

  :commands (org-store-link)
  :config
  ;; Make windmove work in org-mode
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  ;; Show indents
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-agenda-files '("~/Dropbox/Notes"))

  ;; Set this so isearch doesn't hang
  (setq-default search-invisible t)

  ;; Don't expand links by default
  (setq org-descriptive-links t)

  ;; Prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil)

  (use-package ox-pandoc
    :config
    (setq org-pandoc-options-for-markdown '((atx-headers . t))
          org-pandoc-options-for-markdown_mmd '((atx-headers . t))
          org-pandoc-options-for-markdown_github '((atx-headers . t))))

  ;; Create reveal js presentations in org mode.
  (use-package ox-reveal
    :init
    (setq org-reveal-root (concat "file://" (expand-file-name "~/Dropbox/Backups/Reveal/reveal.js")))
    ;; Use htmlize to highlight source code block using my emacs theme
    (use-package htmlize))

  (use-package org-mac-link
    :commands org-mac-grab-link)

  (use-package org-bullets
    :init (add-hook 'org-mode-hook 'org-bullets-mode))

  ;; Start up fully open
  (setq org-startup-folded nil)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Allow bind in files to enable export overrides
  (setq org-export-allow-bind-keywords t)
  (defun grass/html-filter-remove-src-blocks (text backend info)
    "Remove source blocks from html export."
    (when (org-export-derived-backend-p backend 'html) ""))

  ;; Code blocks
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
       (js . t)
       (ruby . t)
       (dot . t)
       (shell . t)))

  ;; Highlight source blocks
  (setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil)

  (require 'org-crypt)
  ;; Automatically encrypt entries tagged `crypt` on save.
  ;; (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  ;; GPG key to use for encryption
  (setq org-crypt-key "ray.grasso@gmail.com")
  (setq org-crypt-disable-auto-save nil)

  (defhydra hydra-org-promote ()
    "promote demote subtrees"
    ("<" 'org-promote-subtree "promote")
    (">" 'org-demote-subtree "demote")
    ("q" nil "quit" :color blue))

  (defhydra hydra-org-move (:color red :columns 3)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t))

    ;; Let me open lines above again
    (evil-define-key 'normal evil-org-mode-map
      "O" 'evil-open-above
      "-" 'dired-jump)

    (general-define-key :keymaps 'org-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "C-,"
      "d" 'grass/insert-org-date-header
      "m" 'hydra-org-move/body
      "g" 'org-mac-grab-link
      "a" 'org-agenda
      "o" 'org-insert-heading
      "i" 'org-insert-link
      "e" '(org-export-dispatch :which-key "export")
      "E" '(:ignore t :which-key "org encrypt")
      "Ee" 'org-encrypt-entry
      "EE" 'org-encrypt-entries
      "Ed" 'org-decrypt-entry
      "ED" 'org-decrypt-entries
      "p" 'hydra-org-promote/body
      "l" 'org-toggle-link-display
      "I" 'org-toggle-inline-images
      "s" 'org-sort-entries
      "t" 'org-todo
      "T" 'org-set-tags
      "c" 'org-cycle-agenda-files)

  (add-hook 'org-mode-hook
    (lambda ()
      ;; No auto indent please
      (setq org-export-html-postamble nil)


      (diminish 'org-indent-mode)

      ;; Add some custom surrounds
      (push '(?e . ("#+BEGIN_EXAMPLE" . "#+END_EXAMPLE")) evil-surround-pairs-alist)
      (push '(?s . ("#+BEGIN_SRC" . "#+END_SRC")) evil-surround-pairs-alist)
      (push '(?q . ("#+BEGIN_QUOTE" . "#+END_QUOTE")) evil-surround-pairs-alist)

      ;; Encrypt on save
      (add-hook 'before-save-hook 'org-encrypt-entries nil t)

      ;; Fix tab key conflict
      (setq-local yas/trigger-key [tab])
      (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand))))

(use-package pandoc-mode
  :config
  (progn
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)))



;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

(use-package ruby-end
  :commands ruby-end-mode)

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
    :diminish (projectile-rails-mode . " ⇋")
    :init
    (progn
      (add-hook 'projectile-mode-hook 'projectile-rails-on))
    :config
    (progn
      (general-define-key :keymaps 'enh-ruby-mode-map
        :states '(normal visual insert emacs)
        :prefix grass/leader2
        :non-normal-prefix "M-,"
        "r" '(:ignore t :which-key "rails")

        "rf" '(:ignore t :which-key "find files")
        "rfa" 'projectile-rails-find-locale
        "rfc" 'projectile-rails-find-controller
        "rfe" 'projectile-rails-find-environment
        "rff" 'projectile-rails-find-feature
        "rfh" 'projectile-rails-find-helper
        "rfi" 'projectile-rails-find-initializer
        "rfj" 'projectile-rails-find-javascript
        "rfl" 'projectile-rails-find-lib
        "rfm" 'projectile-rails-find-model
        "rfn" 'projectile-rails-find-migration
        "rfo" 'projectile-rails-find-log
        "rfp" 'projectile-rails-find-spec
        "rfr" 'projectile-rails-find-rake-task
        "rfs" 'projectile-rails-find-stylesheet
        "rft" 'projectile-rails-find-test
        "rfu" 'projectile-rails-find-fixture
        "rfv" 'projectile-rails-find-view
        "rfy" 'projectile-rails-find-layout
        "rf@" 'projectile-rails-find-mailer

        "rg" '(:ignore t :which-key "goto file")
        "rgc" 'projectile-rails-find-current-controller
        "rgd" 'projectile-rails-goto-schema
        "rge" 'projectile-rails-goto-seeds
        "rgh" 'projectile-rails-find-current-helper
        "rgj" 'projectile-rails-find-current-javascript
        "rgg" 'projectile-rails-goto-gemfile
        "rgm" 'projectile-rails-find-current-model
        "rgn" 'projectile-rails-find-current-migration
        "rgp" 'projectile-rails-find-current-spec
        "rgr" 'projectile-rails-goto-routes
        "rgs" 'projectile-rails-find-current-stylesheet
        "rgt" 'projectile-rails-find-current-test
        "rgu" 'projectile-rails-find-current-fixture
        "rgv" 'projectile-rails-find-current-view
        "rgz" 'projectile-rails-goto-spec-helper
        "rg." 'projectile-rails-goto-file-at-point
        ;; Rails external commands
        "r:" 'projectile-rails-rake
        "rcc" 'projectile-rails-generate
        "ri" 'projectile-rails-console
        "rxs" 'projectile-rails-server
        ;; Refactoring 'projectile-rails-mode
        "rRx" 'projectile-rails-extract-region)
      ;; Ex-commands
      (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)))

  (defun grass/toggle-ruby-block-style ()
    (interactive)
    (enh-ruby-beginning-of-block)
    (if (looking-at-p "{")
      (let ((beg (point)))
        (delete-char 1)
        (insert (if (looking-back "[^ ]") " do" "do"))
        (when (looking-at "[ ]*|.*|")
          (search-forward-regexp "[ ]*|.*|" (line-end-position)))
        (insert "\n")
        (goto-char (- (line-end-position) 1))
        (delete-char 1)
        (insert "\nend")
        (evil-indent beg (point))
        )
      (progn
        (ruby-end-of-block)
        ;; Join lines if block is 1 line of code long
        (save-excursion
          (let ((end (line-end-position)))
            (enh-ruby-beginning-of-block)
            (if (= 2 (- (line-number-at-pos end) (line-number-at-pos)))
              (evil-join (point) end)))
          (kill-line)
          (insert " }")
          (enh-ruby-beginning-of-block)
          (delete-char 2)
          (insert "{" )))))

  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")

  (general-define-key :keymaps 'enh-ruby-mode-map
    :states '(normal visual insert emacs)
    :prefix grass/leader2
    :non-normal-prefix "M-,"
    "b" '(grass/toggle-ruby-block-style :which-key "toggle block")
    "s" 'ruby-switch-to-inf
    "r" 'ruby-send-region
    "l" 'ruby-load-file
    "i" 'inf-ruby

    "t" '(:ignore t :which-key "rspec")
    "ta" 'rspec-verify-all
    "tb" 'rspec-verify
    "tc" 'rspec-verify-continue
    "te" 'rspec-toggle-example-pendingness
    "tf" 'rspec-verify-method
    "tl" 'rspec-run-last-failed
    "tm" 'rspec-verify-matching
    "tr" 'rspec-rerun
    "tt" 'rspec-verify-single)

  ;; Add Ruby block text objects
  (evil-define-text-object evil-inner-ruby-block (count &optional beg end type)
    "Select a Ruby block."
    :extend-selection nil
    (evil-select-paren
      (concat ruby-block-beg-re "\\s-+.*\n?")
      (concat "^\s*" ruby-block-end-re)
      beg
      end
      type
      count
      nil))

  (evil-define-text-object evil-outer-ruby-block (count &optional beg end type)
    "Select a Ruby block."
    :extend-selection nil
    (evil-select-paren
      (concat ruby-block-beg-re "\\s-+")
      ruby-block-end-re
      beg
      end
      type
      count
      t))

  (define-key evil-inner-text-objects-map "r" 'evil-inner-ruby-block)
  (define-key evil-outer-text-objects-map "r" 'evil-outer-ruby-block)

  (add-hook 'enh-ruby-mode-hook
    (lambda ()
      ;; turn off the annoying input echo in irb
      (setq comint-process-echoes t)

      (set (make-variable-buffer-local 'ruby-end-insert-newline) nil)
      ;; Indentation
      (setq ruby-indent-level 2)
      (setq ruby-deep-indent-paren nil)
      (setq enh-ruby-bounce-deep-indent t)
      (setq enh-ruby-hanging-brace-indent-level 2)
      (setq enh-ruby-indent-level 2)
      (setq enh-ruby-deep-indent-paren nil)

      ;; Abbrev mode seems broken for some reason
      (abbrev-mode -1))))

(if (string= system-name "smithy")
  (progn

    (use-package rbenv
      :init
      (progn
        ;; No bright red version in the modeline thanks
        (setq rbenv-modeline-function 'rbenv--modeline-plain)

        (defun grass/enable-rbenv ()
          "Enable rbenv, use .ruby-version if exists."
          (require 'rbenv)

          (let ((version-file-path (rbenv--locate-file ".ruby-version")))
            (global-rbenv-mode)
            ;; try to use the ruby defined in .ruby-version
            (if version-file-path
              (progn
                (rbenv-use (rbenv--read-version-from-file
                             version-file-path))
                (message (concat "[rbenv] Using ruby version "
                           "from .ruby-version file.")))
              (message "[rbenv] Using the currently activated ruby."))))
        (add-hook 'ruby-mode-hook #'grass/enable-rbenv)
        (add-hook 'enh-ruby-mode-hook #'grass/enable-rbenv))))
  (progn
    (use-package chruby
      :init
      (progn
        (defun grass/enable-chruby ()
          "Enable chruby, use .ruby-version if exists."
          (let ((version-file-path (chruby--locate-file ".ruby-version")))
            (chruby)
            ;; try to use the ruby defined in .ruby-version
            (if version-file-path
              (progn
                (chruby-use (chruby--read-version-from-file
                              version-file-path))
                (message (concat "[chruby] Using ruby version "
                           "from .ruby-version file.")))
              (message "[chruby] Using the currently activated ruby."))))

        (add-hook 'ruby-mode-hook #'grass/enable-chruby)
        (add-hook 'enh-ruby-mode-hook #'grass/enable-chruby)))))


;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;

(use-package js2-mode
  :mode  (("\\.js$" . js2-jsx-mode)
           ("\\.jsx?$" . js2-jsx-mode))
  :interpreter "node"
  :config

  (use-package prettier-js
    :diminish prettier-js-mode
    :init
    (setq prettier-js-args '("--trailing-comma" "es5"))
    ;; No format on save for now
    ;; (add-hook 'js2-mode-hook 'prettier-js-mode)
    )

  (use-package js2-refactor
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)

    (general-define-key :keymaps 'js2-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"

      "f" '(:ignore t :which-key "Format")
      "ff" 'prettier-js

      "r" '(:ignore t :which-key "Refactor")

      "r3" '(:ignore t :which-key "ternary")
      "r3i" 'js2r-ternary-to-if

      "r3" '(:ignore t :which-key "add/args")
      "rag" 'js2r-add-to-globals-annotation
      "rao" 'js2r-arguments-to-object

      "rb" '(:ignore t :which-key "barf")
      "rba" 'js2r-forward-barf

      "rb" '(:ignore t :which-key "contract")
      "rca" 'js2r-contract-array
      "rco" 'js2r-contract-object
      "rcu" 'js2r-contract-function

      "re" '(:ignore t :which-key "expand/extract")
      "rea" 'js2r-expand-array
      "ref" 'js2r-extract-function
      "rem" 'js2r-extract-method
      "reo" 'js2r-expand-object
      "reu" 'js2r-expand-function
      "rev" 'js2r-extract-var

      "ri" '(:ignore t :which-key "inline/inject/introduce")
      "rig" 'js2r-inject-global-in-iife
      "rip" 'js2r-introduce-parameter
      "riv" 'js2r-inline-var

      "rl" '(:ignore t :which-key "localize/log")
      "rlp" 'js2r-localize-parameter
      "rlt" 'js2r-log-this

      "rk" '(:ignore t :which-key "kill")
      "rkk" 'js2r-kill

      "rr" '(:ignore t :which-key "rename")
      "rrv" 'js2r-rename-var

      "rs" '(:ignore t :which-key "split/slurp")
      "rsl" 'js2r-forward-slurp
      "rss" 'js2r-split-string
      "rsv" 'js2r-split-var-declaration

      "rt" '(:ignore t :which-key "toggle")
      "rtf" 'js2r-toggle-function-expression-and-declaration

      "ru" '(:ignore t :which-key "unwrap")
      "ruw" 'js2r-unwrap

      "rv" '(:ignore t :which-key "var")
      "rvt" 'js2r-var-to-this

      "rw" '(:ignore t :which-key "wrap")
      "rwi" 'js2r-wrap-buffer-in-iife
      "rwl" 'js2r-wrap-in-for-loop))

  (setq js2-bounce-indent-p t)
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

  ;; Rely on flycheck instead...
  (setq js2-show-parse-errors nil)
  ;; Reduce the noise
  (setq js2-strict-missing-semi-warning nil)
  ;; jshint does not warn about this now for some reason
  (setq js2-strict-trailing-comma-warning nil)

  ;; Quiet warnings
  (setq js2-mode-show-strict-warnings nil)

  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

  (add-hook 'js2-mode-hook
    (lambda ()
      (flycheck-mode 1)
      (setq mode-name "JS2")
      (setq js2-global-externs '("module" "require" "buster" "jestsinon" "jasmine" "assert"
                                  "it" "expect" "describe" "beforeEach"
                                  "refute" "setTimeout" "clearTimeout" "setInterval"
                                  "clearInterval" "location" "__dirname" "console" "JSON")))))

(use-package flow-minor-mode
  :commands (flow-minor-enable-automatically
              flow-minor-mode
              flow-minor-status
              flow-minor-coverage
              flow-minor-type-at-pos
              flow-minor-suggest)
  :config
  (use-package flycheck-flow)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-flow 'rjsx-mode)
    (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
    (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))


(use-package rjsx-mode
  :mode  (("\\.jsx?$" . rjsx-mode)
          ("components\\/.*\\.js\\'" . rjsx-mode))
  :config

  ;; Rely on flycheck instead...
  (setq js2-show-parse-errors nil)
  ;; Reduce the noise
  (setq js2-strict-missing-semi-warning nil)
  ;; jshint does not warn about this now for some reason
  (setq js2-strict-trailing-comma-warning nil)

  ;; Quiet warnings
  (setq js2-mode-show-strict-warnings nil)

  ;; Clear out tag helper
  (define-key rjsx-mode-map "<" nil))

(use-package json-mode
  :mode "\\.json$"
  :config
  (general-define-key :keymaps 'json-mode-map
    :states '(normal visual insert emacs)
    :prefix grass/leader2
    :non-normal-prefix "M-,"
    "p" 'json-pretty-print-buffer
    "j" 'jq-interactively)

  (use-package jq-mode
    :commands jq-interactively)

  (use-package flymake-json
    :init
    (add-hook 'json-mode 'flymake-json-load)))

(use-package typescript-mode
  :mode "\\.ts$"
  :config
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 2)
  (use-package tss
    :init
    (setq tss-popup-help-key ", h")
    (setq tss-jump-to-definition-key ", j")
    (setq tss-implement-definition-key ", i")
    (tss-config-default)))

;; Install elm-format, elm-oracle, and the base elm package
(use-package elm-mode
  :mode "\\.elm$"
  :config

  (use-package flycheck-elm
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)

  (general-define-key :keymaps 'elm-mode-map
    :states '(normal visual insert emacs)
    :prefix grass/leader2
    :non-normal-prefix "M-,"
    "f" 'elm-mode-format-buffer
    "i" 'elm-import
    "c" 'elm-compile-buffer
    "C" 'elm-compile-main
    "l" 'elm-repl-load
    "p" 'elm-repl-push
    "T" 'elm-mode-generate-tags
    "d" 'elm-oracle-doc-at-point
    "t" 'elm-oracle-type-at-point)

  (general-define-key :keymaps 'elm-interactive-mode
    :states '(normal visual insert emacs)
    "<M-up>" 'comint-previous-input
    "<M-down>" 'comint-next-input)

  (diminish 'elm-indent-mode " ⇥")

  (add-hook 'elm-mode-hook
    (lambda ()

      (flycheck-mode 1)
      ;; Fancy indenting please
      (setq tab-always-indent t)
      (setq evil-shift-width 4)
      (setq tab-width 4)
      (setq elm-indent-offset 4))))


;;;;;;;;;;;;
;; Coffee ;;
;;;;;;;;;;;;

(defun grass/indent-relative (&optional arg)
  "Newline and indent same number of spaces as previous line."
  (interactive)
  (let* ((indent (+ 0 (save-excursion
                        (back-to-indentation)
                        (current-column)))))
    (newline 1)
    (insert (make-string indent ?\s))))

(use-package coffee-mode
  :mode  "\\.coffee$"
  :config
  (progn
    ;; Proper indents when we evil-open-below etc...
    (defun grass/coffee-indent ()
      (if (coffee-line-wants-indent)
        ;; We need to insert an additional tab because the last line was special.
        (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
        ;; Otherwise keep at the same indentation level
        (coffee-insert-spaces (coffee-previous-indent))))

    ;; Override indent for coffee so we start at the same indent level
    (defun grass/coffee-indent-line ()
      "Indent current line as CoffeeScript."
      (interactive)
      (let* ((curindent (current-indentation))
              (limit (+ (line-beginning-position) curindent))
              (type (coffee--block-type))
              indent-size
              begin-indents)
        (if (and type (setq begin-indents (coffee--find-indents type limit '<)))
          (setq indent-size (coffee--decide-indent curindent begin-indents '>))
          (let ((prev-indent (coffee-previous-indent))
                 (next-indent-size (+ curindent coffee-tab-width)))
            (if (= curindent 0)
              (setq indent-size prev-indent)
              (setq indent-size (+ curindent coffee-tab-width) ))
            (coffee--indent-insert-spaces indent-size)))))

    (add-hook 'coffee-mode-hook
      (lambda ()
        (set (make-local-variable 'tab-width) 2)
        (setq indent-line-function 'grass/coffee-indent-line)))))

;;;;;;;;;
;; Web ;;
;;;;;;;;;

(use-package web-mode
  :mode  (("\\.html?\\'"    . web-mode)
           ("\\.erb\\'"      . web-mode)
           ("\\.ejs\\'"      . web-mode)
           ("\\.eex\\'"      . web-mode)
           ("\\.handlebars\\'" . web-mode)
           ("\\.hbs\\'"        . web-mode)
           ("\\.eco\\'"        . web-mode)
           ("\\.ect\\'"      . web-mode)
           ("\\.as[cp]x\\'"  . web-mode)
           ("\\.mustache\\'" . web-mode)
           ("\\.dhtml\\'"    . web-mode))
  :config
  (progn

    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
        ad-do-it))

    (general-define-key :keymaps 'web-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"
      "z" 'web-mode-fold-or-unfold)

    (defun grass/web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-comment-keywords t)
      ;; Use server style comments
      (setq web-mode-comment-style 2)
      (setq web-mode-enable-current-element-highlight t))
    (add-hook 'web-mode-hook  'grass/web-mode-hook)))

;; Setup for jsx
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(use-package slim-mode
  :mode "\\.slim$")

(use-package jade-mode
  :mode "\\.jade$"
  :config
  (require 'sws-mode)
  (require 'stylus-mode)
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode)))

(use-package scss-mode
  :mode (("\\.scss$"  . scss-mode)
          ("\\.sass$" . scss-mode))
  :config
  (use-package rainbow-mode)
  (add-hook 'scss-mode-hook
    (lambda ()
      ;; Treat dollar and hyphen as a word character
      (modify-syntax-entry ?$ "w")
      (modify-syntax-entry ?- "w")
      (display-line-numbers-mode)
      (rainbow-mode +1))))

(use-package syslog-mode
  :defer t
  :config
  (add-hook 'syslog-mode-hook
    (lambda ()
      (toggle-truncate-lines +1))))

(use-package css-mode
  :mode "\\.css$"
  :config
  (use-package rainbow-mode)
  (add-hook 'css-mode-hook
    (lambda ()
      (display-line-numbers-mode)
      (rainbow-mode +1))))

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
          ("\\.md$" . markdown-mode))
  :config
  (use-package pandoc-mode
    :commands pandoc-mode
    :diminish pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode)

  (use-package markdown-toc
    :commands markdown-toc-generate-toc)

  (defun grass/markdown-open-in-marked-app ()
    "Run Marked.app on the current file"
    (interactive)
    (shell-command
      (format "open -a 'Marked 2' %s"
        (shell-quote-argument (buffer-file-name)))))

  (defun grass/markdown-enter-key-dwim ()
    "If in a list enter a new list item, otherwise insert enter key as normal."
    (interactive)
    (let ((bounds (markdown-cur-list-item-bounds)))
      (if bounds
        ;; In a list
        (call-interactively #'markdown-insert-list-item)
        ;; Not in a list
        (markdown-enter-key))))

  (define-key markdown-mode-map (kbd "RET") 'grass/markdown-enter-key-dwim)

  ;; Keep word movement instead of promotion mappings
  (define-key markdown-mode-map (kbd "<M-right>") nil)
  (define-key markdown-mode-map (kbd "<M-left>") nil)

  (setq markdown-imenu-generic-expression
    '(("title"  "^\\(.*\\)[\n]=+$" 1)
       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
       ("h1"   "^# \\(.*\\)$" 1)
       ("h2"   "^## \\(.*\\)$" 1)
       ("h3"   "^### \\(.*\\)$" 1)
       ("h4"   "^#### \\(.*\\)$" 1)
       ("h5"   "^##### \\(.*\\)$" 1)
       ("h6"   "^###### \\(.*\\)$" 1)
       ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

  (defhydra hydra-markdown-promote ()
    "promote demote headings"
    ("<" markdown-promote "promote")
    (">" markdown-demote "demote")
    ("q" nil "quit" :color blue))

  (add-hook 'markdown-mode-hook
    (lambda ()
      ;; Remove for now as they interfere with indentation
      ;; (define-key yas-minor-mode-map [(tab)] nil)
      ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
      (setq imenu-generic-expression markdown-imenu-generic-expression)))

  (general-define-key :keymaps 'markdown-mode-map
    :states '(normal visual insert emacs)
    :prefix grass/leader2
    :non-normal-prefix "M-,"
    "p" 'grass/markdown-open-in-marked-app
    "P" 'pandoc-main-hydra/body
    "t" '(markdown-toc-generate-toc :which-key "Generate table of contents")
    "h" 'hydra-markdown-promote/body
    "c" '(mmm-parse-buffer :which-key "highlight code blocks")
    "n" 'markdown-cleanup-list-numbers
    "e" '(markdown-export-and-preview :which-key "export")))


;;;;;;;;;;;
;; Emoji ;;
;;;;;;;;;;;

(use-package emojify
  :commands emojify-insert-emoji
  :init
  (progn
    (add-hook 'org-mode-hook 'emojify-mode)
    (add-hook 'markdown-mode-hook 'emojify-mode)))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)


;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

;; Install some useful packages so this all works
;; stack install intero hlint stylish-haskell hoogle

(use-package haskell-mode
  :defer t
  :config
  (progn
    (setq haskell-mode-maps '(haskell-mode-map literate-haskell-mode-map))

    ;; Set interpreter to be "stack ghci"
    (setq haskell-process-type 'ghci)
    (setq haskell-process-path-ghci "stack")
    (setq haskell-process-args-ghci '("ghci"))

    (setq haskell-process-suggest-remove-import-lines t
      ;; haskell-process-log t
      haskell-process-auto-import-loaded-modules nil
      haskell-process-suggest-remove-import-lines nil
      haskell-tags-on-save nil
      haskell-enable-ghc-mod-support nil
      haskell-enable-ghci-ng-support nil
      haskell-indentation-disable-show-indentations t)

    (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)

    ;; Use hi2 for indentation
    (use-package hi2
      :diminish (hi2-mode . " ⇥")
      :init
      (setq hi2-show-indentations nil)
      (add-hook 'haskell-mode-hook 'turn-on-hi2))

    (use-package intero
      :diminish (intero-mode . " λ")
      :init
      ;; (add-to-list 'company-backends-haskell-mode
      ;;   '(company-intero company-dabbrev-code company-yasnippet))
      (add-hook 'haskell-mode-hook
        (lambda ()
          (let ((checkers '(haskell-ghc haskell-stack-ghc)))
            (if (boundp 'flycheck-disabled-checkers)
              (dolist (checker checkers)
                (add-to-list 'flycheck-disabled-checkers checker))
              (setq flycheck-disabled-checkers checkers)))
          (intero-mode))))

    (flycheck-add-next-checker 'intero
      '(warning . haskell-hlint))

    (defun intero/insert-type ()
      (interactive)
      (intero-type-at :insert))

    (defun intero/display-repl ()
      (interactive)
      (let ((buffer (intero-repl-buffer nil)))
        (unless (get-buffer-window buffer 'visible)
          (display-buffer (intero-repl-buffer nil)))))

    (defun intero/pop-to-repl ()
      (interactive)
      (pop-to-buffer (intero-repl-buffer nil)))

    (defun intero/load-repl-stay-buffer ()
      "Load the current file in the REPL, display the REPL, but preserve buffer focus."
      (interactive)
      (let ((buffer (current-buffer)))
        (intero-repl-load)
        (pop-to-buffer buffer)))

    (general-define-key :keymaps 'intero-mode-map
      :states '(normal visual insert emacs)
      "C-]" 'intero-goto-definition)

    (general-define-key :keymaps 'intero-repl-mode-map
      :states '(normal visual insert emacs)
      "<M-up>" 'comint-previous-input
      "<M-down>" 'comint-next-input)

    (dolist (mode-map haskell-mode-maps)
      (general-define-key :keymaps mode-map
        :states '(normal visual insert emacs)
        :prefix grass/leader2
        :non-normal-prefix "M-,"

        "g" 'intero-goto-definition

        "f" 'haskell-mode-stylish-buffer

        "h" '(:ignore t :which-key "Help")
        "hi" 'intero-info
        "ht" '(intero-type-at . "insert type at point")
        "hs" 'intero-apply-suggestions

        "s" '(:ignore t :which-key "Repl")
        "ss" 'intero-repl
        "sl" 'intero-repl-load
        "sL" 'intero/load-repl-stay-buffer
        ))


    (dolist (mode-map (cons 'haskell-cabal-mode-map haskell-mode-maps))
      (general-define-key :keymaps mode-map
        :states '(normal visual insert emacs)
        :prefix grass/leader2
        :non-normal-prefix "M-,"
        "s" '(:ignore t :which-key "Repl")
        "ss" 'intero-repl
        "sd" 'intero/display-repl
        "sp" 'intero/pop-to-repl))

    (dolist (mode-map (append haskell-mode-maps '(haskell-cabal-mode intero-repl-mode)))
      (general-define-key :keymaps 'haskell-mode-map
        :states '(normal visual insert emacs)
        :prefix grass/leader2
        :non-normal-prefix "M-,"
        "i" '(:ignore t :which-key "Intero")
        "ic" 'intero-cd
        "id" 'intero-devel-reload
        "ik" 'intero-destroy
        "il" 'intero-list-buffers
        "ir" 'intero-restart
        "iT" 'intero/insert-type
        "it" 'intero-targets))

    (add-hook 'haskell-mode-hook
      (lambda ()
        ;; Fancy indenting please
        (setq tab-always-indent t)))))


;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(use-package clojure-mode
  :commands clojure-mode
  :defer t
  :config

  (use-package flycheck-clojure
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup)))

  ;; (add-hook 'clojure-mode-hook
  ;;   (lambda ()
  ;;     ;; Treat dash as part of a word
  ;;     (modify-syntax-entry ?- "w")))

  (use-package clojure-snippets
    :init
    (clojure-snippets-initialize))

  (use-package cider
    :pin melpa-stable
    :init
    ;; REPL history file
    (setq cider-repl-history-file "~/.emacs.d/cider-history")

    ;; Nice pretty printing
    (setq cider-repl-use-pretty-printing t)

    ;; Nicer font lock in REPL
    (setq cider-repl-use-clojure-font-lock t)

    ;; Result prefix for the REPL
    (setq cider-repl-result-prefix ";; => ")

    ;; Don't pop to the REPL window on start
    (setq cider-repl-pop-to-buffer-on-connect 'display-only)

    ;; Neverending REPL history
    (setq cider-repl-wrap-history t)

    ;; Looong history
    (setq cider-repl-history-size 3000)

    ;; Error buffer not popping up
    (setq cider-show-error-buffer nil)

    ;; Highlight sexp in file from REPL
    (use-package cider-eval-sexp-fu)

    ;; eldoc for clojure
    (add-hook 'cider-mode-hook #'eldoc-mode)

    (use-package clj-refactor
      :pin melpa-stable
      :init
      (add-hook 'clojure-mode-hook
        (lambda ()
          (clj-refactor-mode 1)

          ;; no auto sort
          (setq cljr-auto-sort-ns nil)

          ;; do not prefer prefixes when using clean-ns
          (setq cljr-favor-prefix-notation nil))))

    (define-clojure-indent
      ;; Compojure
      (ANY 2)
      (DELETE 2)
      (GET 2)
      (HEAD 2)
      (POST 2)
      (PUT 2)
      (context 2)
      (defroutes 'defun)
      ;; Cucumber
      (After 1)
      (Before 1)
      (Given 2)
      (Then 2)
      (When 2)
      ;; Schema
      (s/defrecord 2)
      ;; test.check
      (for-all 'defun))

    (general-define-key :keymaps 'cider-repl-mode-map
      :states '(normal visual insert emacs)
      "<M-up>" 'cider-repl-previous-input
      "<M-down>" 'cider-repl-next-input)

    (general-define-key :keymaps '(clojure-mode-map cider-repl-mode-map)
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"

      "h" '(:ignore t :which-key "Help")
      "hh" 'cider-doc
      "hg" 'cider-grimoire
      "hj" 'cider-javadoc

      "c" '(:ignore t :which-key "Cider")
      "cj" 'cider-jack-in
      "cC" 'cider-repl-clear-buffer
      "cq" 'cider-quit
      "cc" 'cider-switch-to-repl-buffer

      "," 'cider-repl-handle-shortcut
      "n" 'cider-repl-set-ns
      "s" 'cider-switch-to-last-clojure-buffer

      "e" '(:ignore t :which-key "Eval")
      "ee" 'cider-eval-last-sexp
      "ef" 'cider-eval-defun-at-point
      "er" 'cider-eval-region
      "ew" 'cider-eval-last-sexp-and-replace

      "l" '(:ignore t :which-key "Load")
      "lb" 'cider-load-buffer
      "lf" 'cider-load-file
      "lr" 'cider-refresh

      "g" '(:ignore t :which-key "Goto")
      "gb" 'cider-jump-back
      "ge" 'cider-jump-to-compilation-error
      "gg" 'cider-find-var
      "gr" 'cider-jump-to-resource

      "t" '(:ignore t :which-key "Test")
      "tn" 'cider-test-run-ns-tests
      "tl" 'cider-test-run-test
      "tt" 'cider-test-rerun-test
      "tp" 'cider-test-run-project-tests
      "tr" 'cider-test-show-report

      "r" '(:ignore t :which-key "Refactoring")
      "r?"  'cljr-describe-refactoring

      "ra" '(:ignore t :which-key "Add")
      "rap" 'cljr-add-project-dependency
      "ras" 'cljr-add-stubs

      "rc" '(:ignore t :which-key "Cycle")
      "rcc" 'cljr-cycle-coll
      "rci" 'cljr-cycle-if
      "rcp" 'cljr-cycle-privacy

      "rd" '(:ignore t :which-key "Desctructure")
      "rdk" 'cljr-destructure-keys

      "re" '(:ignore t :which-key "Expand")
      "rel" 'cljr-expand-let

      "rf" '(:ignore t :which-key "Find")
      "rfu" 'cljr-find-usages

      "rh" '(:ignore t :which-key "Hotload")
      "rhd" 'cljr-hotload-dependency

      "ri" '(:ignore t :which-key "Introduce")
      "ril" 'cljr-introduce-let

      "rm" '(:ignore t :which-key "Move")
      "rml" 'cljr-move-to-let

      "rp" '(:ignore t :which-key "Project")
      "rpc" 'cljr-project-clean

      "rr" '(:ignore t :which-key "Remove")
      "rrl" 'cljr-remove-let

      "rs" '(:ignore t :which-key "Sort/Show")
      "rsp" 'cljr-sort-project-dependencies
      "rsc" 'cljr-show-changelog

      "rt" '(:ignore t :which-key "Thread")
      "rtf" 'cljr-thread-first-all
      "rth" 'cljr-thread
      "rtl" 'cljr-thread-last-all

      "ru" '(:ignore t :which-key "Unwind/Update")
      "rua" 'cljr-unwind-all
      "rup" 'cljr-update-project-dependencies
      "ruw" 'cljr-unwind)))

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
;; This was a little too eager
;; (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


;;;;;;;;;;;
;; Elisp ;;
;;;;;;;;;;;


;; Easier navigation of my .init.el
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;; \\(.+\\) ;;$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (general-define-key :keymaps 'emacs-lisp-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"
      "p" 'eval-print-last-sexp)

    (general-define-key :keymaps 'emacs-lisp-mode-map
       "C-c C-e" 'eval-print-last-sexp)))


(use-package elixir-mode
  :mode (("\\.exs?\\'"   . elixir-mode))
  :defer t
  :config
  (add-to-list 'elixir-mode-hook
    (defun auto-activate-ruby-end-mode-for-elixir-mode ()
      (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
        "\\(?:^\\|\\s-+\\)\\(?:do\\)")
      (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
      (ruby-end-mode +1)))

  (defun elixir--umbrella-root (&optional dir)
    (let ((start-dir (or dir (expand-file-name default-directory))))
      (or
        (let* ((proj-dir (locate-dominating-file start-dir alchemist-project-mix-project-indicator))
                (parent-proj-dir
                  (if (stringp proj-dir)
                    (locate-dominating-file (file-name-directory (directory-file-name proj-dir)) alchemist-project-mix-project-indicator))))
          (if (stringp parent-proj-dir) parent-proj-dir proj-dir))

        (let* ((proj-dir (locate-dominating-file start-dir alchemist-project-hex-pkg-indicator))
                (parent-proj-dir
                  (if (stringp proj-dir)
                    (locate-dominating-file (file-name-directory (directory-file-name proj-dir)) alchemist-project-hex-pkg-indicator))))
          (if (stringp parent-proj-dir) parent-proj-dir proj-dir)))))

  ;; Run elixir-format on save
  (add-hook 'elixir-mode-hook
    (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

  (use-package alchemist
    :diminish (alchemist-mode . " alc")
    :diminish (alchemist-phoenix-mode . " alc-ph")
    :init

    (general-define-key :keymaps 'alchemist-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"
      "e" '(:ignore t :which-key "Eval")
      "el" 'alchemist-eval-current-line
      "eL" 'alchemist-eval-print-current-line
      "er" 'alchemist-eval-region
      "eR" 'alchemist-eval-print-region

      "p" '(:ignore t :which-key "Project")
      "pt" 'alchemist-project-find-test
      "g" '(:ignore t :which-key "File Toggle")
      "gt" 'alchemist-project-toggle-file-and-tests
      "gT" 'alchemist-project-toggle-file-and-tests-other-window

      "h" '(:ignore t :which-key "Help")
      "h:" 'alchemist-help
      "hH" 'alchemist-help-history
      "hh" 'alchemist-help-search-at-point
      "hr" 'alchemist-help-search-marked-region

      "f" '(:ignore t :which-key "Format")
      "ff" 'elixir-format-buffer

      "m" '(:ignore t :which-key "Mix")
      "m:" 'alchemist-mix
      "mc" 'alchemist-mix-compile
      "mx" 'alchemist-mix-run
      "mh" 'alchemist-mix-help

      "s" '(:ignore t :which-key "iex")
      "sc" 'alchemist-iex-compile-this-buffer
      "si" 'alchemist-iex-run
      "sI" 'alchemist-iex-project-run
      "sl" 'alchemist-iex-send-current-line
      "sL" 'alchemist-iex-send-current-line-and-go
      "sm" 'alchemist-iex-reload-module
      "sr" 'alchemist-iex-send-region
      "sR" 'alchemist-iex-send-region-and-go

      "t" '(:ignore t :which-key "Test")
      "ta" 'alchemist-mix-test
      "tb" 'alchemist-mix-test-this-buffer
      "tt" 'alchemist-mix-test-at-point
      "tf" 'alchemist-test-file
      "tn" 'alchemist-test-jump-to-next-test
      "tp" 'alchemist-test-jump-to-previous-test
      "tr" 'alchemist-mix-rerun-last-test

      "c" '(:ignore t :which-key "Compile")
      "cb" 'alchemist-compile-this-buffer
      "cf" 'alchemist-compile-file
      "c:" 'alchemist-compile

      "gg" 'alchemist-goto-definition-at-point
      "," 'alchemist-goto-jump-back)

    ;; Hack to disable company popup in Elixir if hanging
    (eval-after-load "alchemist"
      '(defun alchemist-company--wait-for-doc-buffer ()
         (setf num 50)
         (while (and (not alchemist-company-doc-lookup-done)
                  (> (decf num) 1))
           (sit-for 0.01))))))

;;;;;;;;;;;;;;;;;;;;;
;; Other Languages ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package vagrant
  :commands
  (vagrant-destroy
    vagrant-edit
    vagrant-halt
    vagrant-provision
    vagrant-resume
    vagrant-reload
    vagrant-status
    vagrant-suspend
    vagrant-up))

(use-package puppet-mode
  :defer t)

(use-package powershell
  :defer t
  :mode  (("\\.ps1$" . powershell-mode)
           ("\\.psm$" . powershell-mode)))

(use-package rust-mode
  :defer t)

(use-package toml-mode
  :defer t)

(use-package python
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package terraform-mode
  :defer t)

(use-package haml-mode
  :defer t
  :mode "\\.haml$"
  :config
  (add-hook 'haml-mode-hook
    (lambda ()
      (set (make-local-variable 'tab-width) 2))))

(use-package dockerfile-mode
  :defer t
  :config
  (progn
    (general-define-key :keymaps 'dockerfile-mode
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"
      "b" 'dockerfile-build-buffer)))

(use-package docker-tramp)

;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;

(use-package flyspell
  :defer t
  :commands (flyspell-mode flyspell-goto-next-error)
  :diminish (flyspell-mode . " spl")
  :config
  (setq-default ispell-program-name "aspell")
  ;; Silently save my personal dictionary when new items are added
  (setq ispell-silently-savep t)
  (ispell-change-dictionary "british" t)

  (use-package flyspell-correct-popup
    :init
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))

  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
  (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

  (add-hook 'flyspell-mode-hook
    (lambda ()
      (define-key flyspell-mode-map [(control ?\,)] nil))))

(defhydra hydra-spelling ()
  "spelling"
  ("t" flyspell-mode "toggle")
  ("n" flyspell-goto-next-error "next error")
  ("w" flyspell-correct-word-before-point "correct word")
  ("q" nil "quit" :color blue))

(use-package spaceline
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'bar)
    (setq spaceline-minor-modes-separator "⋅")
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-spacemacs-theme)
    (spaceline-info-mode)))

;;;;;;;;;;;;;;;;;;;;;
;; Fix artist mode ;;
;;;;;;;;;;;;;;;;;;;;;

(defun grass/artist-mode-toggle-emacs-state ()
  (if artist-mode
    (evil-emacs-state)
    (evil-exit-emacs-state)))

(add-hook 'artist-mode-hook #'grass/artist-mode-toggle-emacs-state)

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;


(use-package flycheck
  :diminish (flycheck-mode . " ⓢ")
  :defer 3
  :defines grass/toggle-flycheck-error-list
  :commands
  (flycheck-mode
    flycheck-clear
    flycheck-describe-checker
    flycheck-select-checker
    flycheck-set-checker-executable
    flycheck-verify-setup)
  :config
  (progn
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00011100
          #b00111110
          #b00111110
          #b00111110
          #b00011100
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000)))

    (flycheck-define-error-level 'error
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)

    ;; use local eslint from node_modules before global
    ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
    (defun grass/use-eslint-from-node-modules ()
      (let* ((root (locate-dominating-file
                     (or (buffer-file-name) default-directory)
                     "node_modules"))
              (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                          root))))
        (when (and eslint (file-executable-p eslint))
          (setq-local flycheck-javascript-eslint-executable eslint))))

    (add-hook 'flycheck-mode-hook #'grass/use-eslint-from-node-modules)

    ;; Beware that moving this window with a window manager can mess with tooltips
    (use-package flycheck-pos-tip
      :init
      (with-eval-after-load 'flycheck
        (flycheck-pos-tip-mode)))

    (setq flycheck-display-errors-delay 0.5)))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
    :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
    :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

(defun grass/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
    (quit-window nil window)
    (flycheck-list-errors)))


;;;;;;;;;;;;;;;
;; Proselint ;;
;;;;;;;;;;;;;;;

(with-eval-after-load 'flycheck
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
       (id (one-or-more (not (any " "))))
       (message (one-or-more not-newline)
         (zero-or-more "\n" (any " ") (one-or-more not-newline)))
       line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

;;;;;;;;;;;
;; Shell ;;
;;;;;;;;;;;

(add-hook 'eshell-mode-hook
  (lambda ()
    (use-package eshell-prompt-extras
      :commands epe-theme-lambda
      :init
      (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))))

;;;;;;;;;;;;;;;;;
;; Rest Client ;;
;;;;;;;;;;;;;;;;;

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (progn
    (defun restclient-http-send-current-raw-stay-in-window ()
      (interactive)
      (restclient-http-send-current t t))

    (general-define-key :keymaps 'restclient-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"
      "s" 'restclient-http-send-current-stay-in-window
      "S" 'restclient-http-send-current
      "r" 'restclient-http-send-current-raw-stay-in-window
      "R" 'restclient-http-send-current-raw)))


;;;;;;;;;;;;;;
;; Graphviz ;;
;;;;;;;;;;;;;;

(use-package graphviz-dot-mode
  :config
  (progn
    (defun grass/open-attribute-help ()
      (interactive)
      (browse-url "http://www.graphviz.org/doc/info/attrs.html"))
    (general-define-key :keymaps 'graphviz-dot-mode-map
      :states '(normal visual insert emacs)
      :prefix grass/leader2
      :non-normal-prefix "M-,"
      "c" 'compile
      "p" 'graphviz-dot-preview
      "v" 'graphviz-dot-view
      "h" 'grass/open-attribute-help)))

;;;;;;;;;
;; SQL ;;
;;;;;;;;;

(add-hook 'sql-interactive-mode-hook
  (lambda ()
    (toggle-truncate-lines t)))

(eval-after-load "sql"
  '(progn
     (setq sql-mysql-login-params (append sql-mysql-login-params '(port :default 3306)))
     (setq sql-postgres-login-params (append sql-postgres-login-params '(port :default 5432)))
     (sql-set-product 'postgres)
     (general-define-key :keymaps 'sql-mode-map
       :states '(normal visual insert emacs)
       :prefix grass/leader2
       :non-normal-prefix "M-,"
       "s" '(:ignore t :which-key "Send to REPL")
       "sb" 'sql-send-buffer
       "sf" 'sql-send-paragraph
       "sr" 'sql-send-region
       "i" 'sql-set-sqli-buffer
       "p" 'sql-postgres
       "m" 'sql-mysql
       "r" 'sql-show-sqli-buffer)))

;;;;;;;;;;
;; Tmux ;;
;;;;;;;;;;

(use-package emamux
  :commands (emamux:send-command
              emamux:run-command
              emamux:run-last-command
              emamux:zoom-runner
              emamux:inspect-runner
              emamux:close-runner-pane
              emamux:close-panes
              emamux:clear-runner-history
              emamux:interrupt-runner
              emamux:copy-kill-ring
              emamux:yank-from-list-buffers))

;;;;;;;;;;
;; Epub ;;
;;;;;;;;;;

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(general-define-key
  :states '(normal visual insert emacs)
  :prefix grass/leader1
  :non-normal-prefix "M-SPC"

  "TAB" '(grass/switch-to-previous-buffer :which-key "previous buffer")
  "!" 'eshell
  "~" 'evil-emacs-state
  ":" 'counsel-M-x
  "]" 'hydra-surround/body
  "~" 'hydra-change-case/body
  ";" 'iedit-mode
  "?" 'swiper
  "/" 'swiper-current-word
  "-" 'dired-jump
  "SPC" '(grass/remove-search-highlights :which-key "clear search highlights")

  "<left>" 'evil-window-left
  "<right>" 'evil-window-right
  "<up>" 'evil-window-up
  "<down>" 'evil-window-down

  "c" '(:ignore t :which-key "Check/Compile")
  "cc" '(flycheck-mode :which-key "toggle flycheck")
  "cm" 'hydra-flycheck/body
  "cC" 'flycheck-clear
  "ch" 'flycheck-describe-checker
  "cS" 'flycheck-select-checker
  "cd" 'flycheck-disable-checker
  "cl" '(grass/toggle-flycheck-error-list :which-key "toggle error list")
  "cx" '(flycheck-set-checker-executable :which-key "set checker")
  "cv" 'flycheck-verify-setup

  "cs" '(hydra-spelling/body :which-key "Spelling")

  "cf" '(:ignore t :which-key "Flow")
  "cff" 'flow-minor-mode
  "cfs" 'flow-minor-status
  "cfc" 'flow-minor-coverage
  "cft" 'flow-minor-type-at-pos
  "cfS" 'flow-minor-suggest

  "s" '(:ignore t :which-key "Search/Replace")
  "sc" 'grass/remove-search-highlights
  "sp" 'anzu-query-replace-at-cursor-thing
  "sa" 'counsel-rg
  "sA" 'counsel-ag
  "sg" 'ripgrep-regexp
  "sG" 'counsel-git-grep
  "ss" 'swiper
  "s;" 'iedit-mode
  "si" 'counsel-imenu
  "s:" 'grass/iedit-dwim
  "sr" 'grass/query-replace-regexp-in-entire-buffer
  "sR" 'grass/replace-regexp-in-entire-buffer
  "sf" 'isearch-forward-regexp
  "sF" 'isearch-reverse-regexp
  "sp" '(grass/counsel-rg-current-project :which-key "rg project")
  "sP" 'projectile-ag

  "b" '(:ignore t :which-key "Buffers")
  "bb" 'ivy-switch-buffer
  "bB" 'ivy-switch-buffer-other-window
  "bm" '(hydra-buffer/body :which-key "buffer mini-state")
  "bs" 'grass/switch-to-scratch-buffer
  "bk" 'kill-this-buffer
  "bw" 'kill-buffer-and-window
  "bo" 'crux-kill-other-buffers
  "bi" 'ibuffer

  "k" '(:ignore t :which-key "Bookmarks")
  "ki" 'grass/open-init
  "kw" 'grass/open-work-log
  "kn" 'grass/find-notes
  "kt" 'grass/find-tab

  "g" '(:ignore t :which-key "Git/VC")
  "gs" 'magit-status
  "gb" 'magit-blame
  "gl" 'git-link
  "gc" 'git-link-commit
  "gB" 'github-browse-file
  "gt" 'git-timemachine-toggle
  "gp" 'grass/open-pull-request

  "gL" '(:ignore t :which-key "Magit log...")
  "gLd" 'magit-log-trace-definition
  "gLf" 'magit-log-buffer-file

  "gg" '(:ignore t :which-key "Gist")
  "ggb" 'gist-buffer
  "ggB" 'gist-buffer-private
  "ggl" 'gist-list
  "ggr" 'gist-region
  "ggR" 'gist-region-private

  "h" '(:ignore t :which-key "Help")
  "hf" 'describe-function
  "hv" 'describe-variable
  "hk" 'describe-key
  "ha" 'apropos

  "j" '(:ignore t :which-key "Jump to definition")
  "jj" 'dumb-jump-go
  "jb" 'dumb-jump-back
  "jq" 'dumb-jump-quick-look
  "jo" 'dumb-jump-go-other-window
  "jp" 'dumb-jump-go-prompt
  "jh" 'dumb-jump-go-prefer-external

  "e" '(:ignore t :which-key "Editing/Text")
  "eC" 'counsel-unicode-char
  "ek" 'browse-kill-ring
  "eh" 'hydra-goto-history/body
  "ez" 'zop-up-to-char
  "ef" 'crux-indent-defun
  "ei" 'crux-cleanup-buffer-or-region
  "ew" 'whitespace-cleanup
  "eT" 'untabify
  "et" '(grass/toggle-always-indent :which-key "toggle tab indent")
  "ei" 'hydra-insert-timestamp/body
  "ec" 'char-menu
  "eb" 'grass/comment-box
  "ed" 'grass/insert-date
  "eD" 'grass/insert-datetime
  "ee" 'emojify-insert-emoji
  "es" 'stupid-indent-mode

  "ea" '(:ignore t :which-key "Alignment")
  "eaa" 'align
  "ear" 'align-repeat
  "eam" 'align-repeat-math-oper
  "ea." 'align-repeat-decimal
  "ea," 'align-repeat-comma
  "ea;" 'align-repeat-semicolon
  "ea:" 'align-repeat-colon
  "ea=" 'align-repeat-equal
  "ea>" 'align-repeat-hash
  "ea&" 'align-repeat-ampersand
  "ea|" 'align-repeat-bar
  "ea(" 'align-repeat-left-paren
  "ea)" 'align-repeat-right-paren

  "f" '(:ignore t :which-key "Files")
  "fr" 'counsel-recentf
  "ff" 'counsel-find-file
  "fE" 'epa-encrypt-file
  "fD" 'epa-decrypt-file
  "fR" 'grass/rename-file-and-buffer
  "fc" 'grass/copy-buffer-filename
  "fd" 'crux-delete-file-and-buffer
  "fj" 'dired-jump
  "fs"  '(save-buffer :which-key "save file")

  "t" '(:ignore t :which-key "Terminal/Tmux")
  "tr" 'emamux:run-command
  "tt" 'emamux:run-last-command

  "u" '(:ignore t :which-key "Utilities")
  "uU" 'crux-view-url
  "ud" 'ediff-buffers
  "ut" 'display-time-world
  "uc" 'quick-calc
  "uu" 'browse-url
  "uf" 'reveal-in-osx-finder
  "uw" 'count-words
  "ul" 'org-store-link

  "v" 'er/expand-region

  "y" '(:ignore t :which-key "Snippets")
  "yi" 'yas-insert-snippet
  "ys" 'company-yasnippet
  "yn" 'yas-new-snippet
  "ye" 'hippie-expand

  "w" '(:ignore t :which-key "Windows/UI")
  "wl" '(toggle-truncate-lines :which-key "toggle line wrap")
  "wz" 'hydra-zoom-text/body
  "ww" 'evil-window-next
  "wm" '(hydra-window/body :which-key "window mini state")
  "wv" '((lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) :which-key "Split Vertically")
  "wh" '((lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) :which-key "Split Horizontal")
  "wo" 'delete-other-windows
  "wk" 'delete-window
  "wt" 'crux-transpose-windows
  "wn" 'display-line-numbers-mode
  "wi" 'highlight-indent-guides-mode

  "z" '(:ignore t :which-key "Folding")
  "zz" 'origami-toggle-node
  "zs" 'origami-show-only-node
  "zo" 'origami-open-all-nodes
  "zu" 'origami-undo
  "zr" 'origami-redo
  )

(general-define-key
  "M-Y" 'counsel-yank-pop
  "M-x" 'counsel-M-x
  "C-x C-f" 'counsel-find-file
  "C-c C-r" 'ivy-resume
  "<f6>" 'ivy-resume

  "C-`" 'evil-normal-state
  "C-;" 'iedit-mode

  "<home>" 'move-beginning-of-line
  "<end>" 'move-end-of-line

  "C-x C-j" 'dired-jump

  "s-d" 'crux-duplicate-current-line-or-region

  "C-x C-m" 'counsel-M-x

  "s-P" 'counsel-M-x
  "s-p" 'counsel-projectile-find-file
  "s-e" 'hippie-expand

  "M-/" 'hippie-expand
  "M-z" 'zop-up-to-char)

(general-define-key
  :states '(normal)
  "-" 'dired-jump)

(general-define-key :keymaps 'comint-mode
  :states '(normal visual insert emacs)
  "<M-up>" 'comint-previous-input
  "<M-down>" 'comint-next-input)

(general-define-key :keymaps 'ivy-minibuffer-map
  "RET" 'ivy-alt-done
  "M-y" 'ivy-next-line
  "S-<up>" 'ivy-previous-history-element
  "S-<down>" 'ivy-next-history-element)

(general-nvmap "z" 'origami-recursively-toggle-node)

(general-imap "C-p" 'hippie-expand)

(global-set-key (kbd "<backtab>") 'stupid-outdent)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "<home>") #'crux-move-beginning-of-line)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

(provide 'init)
;;; init.el ends here
