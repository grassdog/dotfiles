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
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                          ("gnu"       . "http://elpa.gnu.org/packages/")
                          ("melpa"     . "https://melpa.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Debug package loads
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some base libraries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bind-key)
(require 'diminish)
(require 'cl)

;; UTF-8 Thanks
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; File paths
(defvar grass/dotfiles-dir (file-name-directory load-file-name)
  "The root dir of my Emacs config.")
(defvar grass/config-dir (expand-file-name "grass" grass/dotfiles-dir)
  "The directory containing configuration files.")
(defvar grass/snippets-dir (expand-file-name "snippets" grass/dotfiles-dir)
  "A house for snippets.")
(defvar grass/savefile-dir (expand-file-name "savefile" grass/dotfiles-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar grass/undo-dir (expand-file-name "undo" grass/dotfiles-dir)
  "Undo files.")

;; Ensure savefile directory exists
(unless (file-exists-p grass/savefile-dir)
  (make-directory grass/savefile-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up load paths
(add-to-list 'load-path grass/config-dir)
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

;; Faster
(setq font-lock-verbose nil)

;; no jerky scrolling
(setq scroll-conservatively 101)

;; Use right alt for extended character insertion
(setq mac-right-option-modifier nil)

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

;; Don't make tab indent a line (set to t if you want fancy tabbing)
(setq tab-always-indent nil)

;; Don't combine tag tables thanks
(setq tags-add-tables nil)

;; Don't pop up new frames on each call to open
(setq ns-pop-up-frames nil)

;; Wrap lines for text modes
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; Lighter line continuation arrows
(define-fringe-bitmap 'left-curly-arrow [0 64 72 68 126 4 8 0])
(define-fringe-bitmap 'right-curly-arrow [0 2 18 34 126 32 16 0])

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

;; TODO Review if this works

(defun grass/set-gui-config ()
  "Enable my GUI settings"
  (interactive)
  (load-theme 'spacemacs-dark t)

  (menu-bar-mode +1)
  ;; Highlight the current line
  (global-hl-line-mode +1))

(defun grass/set-terminal-config ()
  "Enable my terminal settings"
  (interactive)
  (xterm-mouse-mode 1)
  (menu-bar-mode -1)
  (load-theme 'spacemacs-dark t))

(use-package spacemacs-theme)

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
  ;; Don't count candidates
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
    '((swiper . ivy--regex-plus)
       (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package counsel
  :init
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package swiper
  :commands swiper)

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.4)
  (setq which-key-min-display-lines 3)

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


(use-package highlight-indentation
  :commands highlight-indentation-mode)


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
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil (Trojan horse maneuver) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :preface
  (setq evil-search-module 'evil-search)

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

  (use-package evil-search-highlight-persist
    :init
    (setq evil-search-highlight-string-min-len 3)
    (global-evil-search-highlight-persist t)

    (defun grass/remove-search-highlights ()
      "Remove all highlighted search terms."
      (interactive)
      (lazy-highlight-cleanup)
      (evil-search-highlight-persist-remove-all)
      (evil-ex-nohighlight))

    (define-key evil-normal-state-map (kbd "SPC s c") 'grass/remove-search-highlights)

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

    ;; Little word
    (require 'evil-little-word)
    (define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
    (define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
    (define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
    (define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
    (define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
    (define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word)

    (evil-mode t)

    ;; Yank till end of line
    (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

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
    (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
    (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
    ;; (define-key evil-normal-state-map "\C-f" 'evil-forward-char)
    ;; (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
    ;; (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
    ;; (define-key evil-normal-state-map "\C-b" 'evil-backward-char)
    ;; (define-key evil-insert-state-map "\C-b" 'evil-backward-char)
    ;; (define-key evil-visual-state-map "\C-b" 'evil-backward-char)
    ;; (define-key evil-normal-state-map "\C-d" 'evil-delete-char)
    ;; (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
    ;; (define-key evil-visual-state-map "\C-d" 'evil-delete-char)
    ;; (define-key evil-normal-state-map "\C-n" 'evil-next-line)
    ;; (define-key evil-insert-state-map "\C-n" 'evil-next-line)
    ;; (define-key evil-visual-state-map "\C-n" 'evil-next-line)
    ;; (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
    ;; (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
    ;; (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
    ;; (define-key evil-normal-state-map "\C-y" 'yank)
    ;; (define-key evil-insert-state-map "\C-y" 'yank)
    ;; (define-key evil-visual-state-map "\C-y" 'yank)
    ;; (define-key evil-normal-state-map "\C-k" 'kill-line)
    ;; (define-key evil-insert-state-map "\C-k" 'kill-line)
    ;; (define-key evil-visual-state-map "\C-k" 'kill-line)
    (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
    (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)

    ;; Set our default modes
    (loop for (mode . state) in '(
                                   ;;(inferior-emacs-lisp-mode . emacs)
                                   ;; (nrepl-mode . insert)
                                   ;; (pylookup-mode . emacs)
                                   ;; (comint-mode . normal)
                                   ;; (shell-mode . emacs)
                                   ;; (git-commit-mode . insert)
                                   ;; (git-rebase-mode . emacs)
                                   ;; (calculator-mode . emacs)
                                   ;; (term-mode . emacs)
                                   ;; (haskell-interactive-mode . emacs)
                                   ;; (undo-tree-visualizer-mode . emacs)
                                   ;; (cider-repl-mode . emacs)
                                   (cider-stacktrace-mode . motion)
                                   (cider-popup-buffer-mode . motion)
                                   ;; (help-mode . normal)
                                   ;; (grep-mode . emacs)
                                   ;; (bc-menu-mode . emacs)
                                   ;; (erc-mode . emacs)
                                   ;; (magit-branch-manager-mode . emacs)
                                   ;; (magit-blame-mode-map . emacs)
                                   ;; (magit-cherry-mode-map . emacs)
                                   ;; (magit-diff-mode-map . emacs)
                                   ;; (magit-log-mode-map . emacs)
                                   ;; (magit-log-select-mode-map . emacs)
                                   ;; (magit-mode-map . emacs)
                                   ;; (magit-popup-help-mode-map . emacs)
                                   ;;(magit-popup-mode . emacs)
                                   ;;(magit-popup-sequence-mode . emacs)
                                   ;; (magit-process-mode-map . emacs)
                                   ;; (magit-reflog-mode-map . emacs)
                                   ;; (magit-refs-mode-map . emacs)
                                   ;; (magit-revision-mode-map . emacs)
                                   ;; (magit-stash-mode-map . emacs)
                                   ;; (magit-stashes-mode-map . emacs)
                                   ;;(magit-status-mode . emacs)
                                   ;; (rdictcc-buffer-mode . emacs)
                                   ;; (kill-ring-mode . normal)
                                   ;; (bs-mode . emacs)
                                   ;; (dired-mode . normal)
                                   ;; (wdired-mode . normal)
                                   )
      do (evil-set-initial-state mode state))))

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
  (setq drag-stuff-modifier '(control shift))
  (drag-stuff-global-mode 1))


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

(use-package iedit
  :commands 'iedit-mode
  :defines grass/iedit-dwim
  :config
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
            ;; `current-word' can of course be replaced by other functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max))))))))


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


(defun grass/today ()
  (format-time-string "%Y.%m.%d - %a"))

(defun grass/insert-datetime (arg)
  "Without argument: insert date as yyyy-mm-dd
With C-u: insert date and time
With C-u C-u: insert time"
  (interactive "P")
  (cond ((equal arg '(16)) (insert (format-time-string "%T")))
    ((equal arg '(4)) (insert (format-time-string "%Y-%m-%d %T")))
    (t (insert (format-time-string "%Y-%m-%d")))))

(defun grass/insert-date ()
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

;; Quick buffer switch
(defun grass/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

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


;;;;;;;;;
;; Git ;;
;;;;;;;;;

(use-package magit
  :commands magit-status
  :config
  (use-package evil-magit)

  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-link
  :commands (git-link git-link-commit)
  :config
  (setq git-link-open-in-browser t))

(use-package git-timemachine
  :commands git-timemachine)

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


;;;;;;;;;;;;;;;;;;;;;;
;; Symbol insertion ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package char-menu
  :commands char-menu
                                        ; Em-dash is first
  :config (setq char-menu '("—" "‘’" "“”" "…" "«»" "–"
                             ("Typography" "⋅" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                             ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
                             ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto save on focus lost ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (setq dired-omit-verbose nil)
  (setq dired-omit-files
    (rx (or (seq bol (? ".") "#")       ;; emacs autosave files
          (seq "~" eol)                 ;; backup-files
          (seq bol "CVS" eol)           ;; CVS dirs
          (seq ".pyc" eol)
          (seq bol ".DS_Store" eol)
          (seq bol ".tern-port" eol))))

  (general-emacs-define-key dired-mode-map :states '(normal visual) :prefix grass/leader1
    "mg" 'revert-buffer)

  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "^")
    (function
      (lambda nil (interactive) (dired-single-buffer ".."))))
  (define-key dired-mode-map (kbd "<s-up>")
    (function
      (lambda nil (interactive) (dired-single-buffer ".."))))
  (define-key dired-mode-map (kbd "-")
    (function
      (lambda nil (interactive) (dired-single-buffer "..")))))

(if (boundp 'dired-mode-map)
  (grass/dired-init)
  (add-hook 'dired-load-hook 'grass/dired-init))

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-omit-mode t)
    (dired-hide-details-mode t)))

(eval-after-load "dired"
  '(progn
     (use-package dired-single
       :commands 'dired-single-buffer)

     (use-package peep-dired
       :general
       (:keymaps 'dired-mode-map :states '(normal visual) :prefix grass/leader1
         "mp" 'peep-dired))))


(use-package dired+
  :commands (dired-omit-mode dired-jump)
  :config
  ;; Chill the colours in dired
  (setq font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
  (diminish 'dired-omit-mode ""))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and Replace ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Use anzu replace methods in these

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


(defun grass/replace-string-in-entire-buffer (from-string to-string &optional delimited start end)
  "This is a modified version of `replace-string'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
  (interactive
    (let ((common
            (query-replace-read-args
              (concat "Replace"
                (if current-prefix-arg " word" "")
                (if (and transient-mark-mode mark-active) " in region" ""))
              nil)))
      (list (nth 0 common) (nth 1 common) (nth 2 common)
        (if (and transient-mark-mode mark-active)
          (region-beginning)
          (buffer-end -1))
        (if (and transient-mark-mode mark-active)
          (region-end)
          (buffer-end 1)))))
  (perform-replace from-string to-string nil nil delimited nil nil start end))

(defun grass/replace-regexp-in-entire-buffer (regexp to-string &optional delimited start end)
  "This is a modified version of `replace-regexp'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
  (interactive
    (let ((common
            (query-replace-read-args
              (concat "Replace"
                (if current-prefix-arg " word" "")
                " regexp"
                (if (and transient-mark-mode mark-active) " in region" ""))
              t)))
      (list (nth 0 common) (nth 1 common) (nth 2 common)
        (if (and transient-mark-mode mark-active)
          (region-beginning)
          (buffer-end -1))
        (if (and transient-mark-mode mark-active)
          (region-end)
          (buffer-end 1)))))
  (perform-replace regexp to-string nil t delimited nil nil start end))

(defun grass/query-replace-regexp-in-entire-buffer (regexp to-string &optional delimited start end)
  "This is a modified version of `query-replace-regexp'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
  (interactive
    (let ((common
            (query-replace-read-args
              (concat "Replace"
                (if current-prefix-arg " word" "")
                " regexp"
                (if (and transient-mark-mode mark-active) " in region" ""))
              t)))
      (list (nth 0 common) (nth 1 common) (nth 2 common)
        (if (and transient-mark-mode mark-active)
          (region-beginning)
          (buffer-end -1))
        (if (and transient-mark-mode mark-active)
          (region-end)
          (buffer-end 1)))))
  (perform-replace regexp to-string t t delimited nil nil start end))

(defun grass/query-replace-string-in-entire-buffer (from-string to-string &optional delimited start end)
  "This is a modified version of `query-replace-string'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
  (interactive
    (let ((common
            (query-replace-read-args
              (concat "Replace"
                (if current-prefix-arg " word" "")
                (if (and transient-mark-mode mark-active) " in region" ""))
              nil)))
      (list (nth 0 common) (nth 1 common) (nth 2 common)
        (if (and transient-mark-mode mark-active)
          (region-beginning)
          (buffer-end -1))
        (if (and transient-mark-mode mark-active)
          (region-end)
          (buffer-end 1)))))
  (perform-replace from-string to-string t nil delimited nil nil start end))


(use-package ag
  :commands (ag ag-project))

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
(diminish 'abbrev-mode "ⓐ")
(setq-default abbrev-mode t)

(use-package company
  :diminish (company-mode . "ⓒ")
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
     (general-define-key
       :states '(normal visual insert emacs)
       :prefix grass/leader1
       :non-normal-prefix "M-SPC"
       "es" 'company-yasnippet
       "ee" 'hippie-expand
       )
     (general-define-key
       "s-e" 'company-yasnippet)

     (general-define-key :keymaps 'company-active-map
       "TAB" 'company-complete-common-or-cycle
       "<tab>" 'company-complete-common-or-cycle
       "S-TAB" 'company-select-previous
       "<backtab>" 'company-select-previous
       "ESC" 'company-abort)))

(add-hook 'after-init-hook 'global-company-mode)

;; replace dabbrev-expand with Hippie expand
(setq hippie-expand-try-functions-list
  '(
     ;; Try to expand yasnippet snippets based on prefix
     yas-hippie-try-expand hippie-expand-try-functions-list

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
  :diminish (yas-minor-mode . "ⓨ")
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
              corral-braces-forward))

(defhydra hydra-move-parens (:columns 4)
  "Corral"
  ("(" corral-parentheses-backward "Back")
  (")" corral-parentheses-forward "Forward")
  ("[" corral-brackets-backward "Back")
  ("]" corral-brackets-forward "Forward")
  ("{" corral-braces-backward "Back")
  ("}" corral-braces-forward "Forward")
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

(use-package nlinum
  :init
  (setq nlinum-format "%4d ")

  ;; Line numbers for coding please
  (add-hook 'prog-mode-hook
    (lambda ()
      (nlinum-mode 1))))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

;; Line numbers for coding please
(add-hook 'prog-mode-hook
  (lambda ()
    ;; Treat underscore as a word character
    (modify-syntax-entry ?_ "w")
    ;; (linum-mode 1)
    (rainbow-delimiters-mode)))


;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

;; Simple indentation please
(use-package clean-aindent-mode
  :disabled
  :init
                                        ; no electric indent, auto-indent is sufficient
  (electric-indent-mode -1)
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t))

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

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

                                        ; https://gist.github.com/mishoo/5487564
(defcustom stupid-indent-level 2
  "Indentation level for stupid-indent-mode")

(defun grass/stupid-outdent-line ()
  (interactive)
  (let (col)
    (save-excursion
      (beginning-of-line-text)
      (setq col (- (current-column) stupid-indent-level))
      (when (>= col 0)
        (indent-line-to col)))))

(defun grass/stupid-outdent-region (start stop)
  (interactive)
  (setq stop (copy-marker stop))
  (goto-char start)
  (while (< (point) stop)
    (unless (and (bolp) (eolp))
      (grass/stupid-outdent-line))
    (forward-line 1)))

(defun grass/stupid-outdent ()
  (interactive)
  (if (use-region-p)
    (save-excursion
      (grass/stupid-outdent-region (region-beginning) (region-end))
      (setq deactivate-mark nil))
    (grass/stupid-outdent-line)))

(global-set-key (kbd "<backtab>") 'grass/stupid-outdent)


;;;;;;;;;;;;;;;;
;; Whitespace ;;
;;;;;;;;;;;;;;;;

(require 'whitespace)
(diminish 'global-whitespace-mode)
;; Only show bad whitespace (Ignore empty lines at start and end of buffer)
;; (setq whitespace-style '(face tabs trailing space-before-tab indentation space-after-tab))
;; (global-whitespace-mode t)

(setq require-final-newline t)


;; Only trim modified lines on save
(use-package ws-butler
  :defer 2
  :diminish (ws-butler-mode . "ⓦ")
  :config
  (setq ws-butler-keep-whitespace-before-point t)
  (ws-butler-global-mode 1))


;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(use-package projectile
  :diminish (projectile-mode . "ⓟ")
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
  (add-to-list 'projectile-globally-ignored-files "TAGS")

  (use-package counsel-projectile
    :init
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)

      (general-define-key
        :states '(normal visual insert emacs)
        :prefix grass/leader1
        :non-normal-prefix "M-SPC"

        "p" '(:ignore t :which-key "Projectile")
        "p SPC" 'counsel-projectile
        "pb"    'counsel-projectile-switch-to-buffer
        "bp"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file
        "fp"    'counsel-projectile-find-file
        "pr"    'projectile-recentf)

      (defun grass/counsel-ag-current-project ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
            (counsel-ag "" dir)
            (message "error: Not in a project."))))
      :init
      (projectile-global-mode t))))


;;;;;;;;;
;; Org ;;
;;;;;;;;;

(use-package org
  :defer t

  :config
  ;; Make windmove work in org-mode
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  ;; Show indents
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-agenda-files '("~/Dropbox/Notes"))
  ;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil)

  ;; Use pandoc for exports
  (use-package ox-pandoc)

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

  ;; Show raw link text
  (setq org-descriptive-links nil)
  ;; Start up fully open
  (setq org-startup-folded nil)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))

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
       (sh . t)))

  ;; Highlight source blocks
  (setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil)

  (defhydra hydra-org-move (:color red :columns 3)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t))

  (add-hook 'org-mode-hook
    (lambda ()
      ;; No auto indent please
      (setq org-export-html-postamble nil)

      ;; Add some custom surrounds
      (push '(?e . ("#+BEGIN_EXAMPLE" . "#+END_EXAMPLE")) evil-surround-pairs-alist)
      (push '(?s . ("#+BEGIN_SRC" . "#+END_SRC")) evil-surround-pairs-alist)
      (push '(?q . ("#+BEGIN_QUOTE" . "#+END_QUOTE")) evil-surround-pairs-alist)

      (general-define-key :keymaps 'org-mode-map :states '(normal visual) :prefix grass/leader1
        "mm" 'hydra-org-move/body
        "mg" 'org-mac-grab-link
        "ma" 'org-agenda
        "mc" 'org-cycle-agenda-files))))

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

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

  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")

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

      ;; Abbrev mode seems broken for some reason
      (abbrev-mode -1))))

(if (string= system-name "smithy")
  (progn
    (use-package chruby
      :commands chruby-use-corresponding)
    (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding))
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
        (add-hook 'enh-ruby-mode-hook #'grass/enable-rbenv)))))


;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;

(use-package js2-mode
  :mode  (("\\.js$" . js2-jsx-mode)
           ("\\.jsx?$" . js2-jsx-mode)
           ("\\.es6$" . js2-mode))
  :interpreter "node"
  :config
  (use-package js2-refactor
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c RET"))

  (setq js2-bounce-indent-p t)

  ;; Rely on flycheck instead...
  (setq js2-show-parse-errors nil)
  ;; Reduce the noise
  (setq js2-strict-missing-semi-warning nil)
  ;; jshint does not warn about this now for some reason
  (setq js2-strict-trailing-comma-warning nil)

  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

  (add-hook 'js2-mode-hook
    (lambda ()
      (setq mode-name "JS2")
      (setq js2-global-externs '("module" "require" "buster" "jestsinon" "jasmine" "assert"
                                  "it" "expect" "describe" "beforeEach"
                                  "refute" "setTimeout" "clearTimeout" "setInterval"
                                  "clearInterval" "location" "__dirname" "console" "JSON"))

      (flycheck-mode 1)
      (js2-imenu-extras-mode +1))))

(use-package json-mode
  :mode "\\.json$"
  :general
  (:keymaps 'json-mode-map :states '(normal visual) :prefix grass/leader1
    "mp" 'json-pretty-print-buffer)
  :config
  (use-package flymake-json
    :init
    (add-hook 'json-mode 'flymake-json-load))
  (flycheck-mode 1))

(use-package typescript-mode
  :mode "\\.ts$"
  :config
  (setq typescript-indent-level 2)
  (setq typescript-expr-indent-offset 2)
  (use-package tss
    :init
    (setq tss-popup-help-key "SPC m h")
    (setq tss-jump-to-definition-key "SPC m j")
    (setq tss-implement-definition-key "SPC m i")
    (tss-config-default)))

(use-package elm-mode
  :mode "\\.elm$"
  :config

  (use-package flycheck-elm
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))


  (add-hook 'elm-mode-hook
    (lambda ()
      ;; Reenable elm oracle once it's start up cost doesn't smash editor performance
      ;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

      (setq evil-shift-width 4)
      (setq tab-width 4)
      (setq elm-indent-offset 4)
      (flycheck-mode t))))


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
        (flycheck-mode t)
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

(use-package jade-mode
  :mode "\\.jade$"
  :config
  (require 'sws-mode)
  (require 'stylus-mode)
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode)))

(use-package scss-mode
  :mode "\\.scss$"
  :config
  (use-package rainbow-mode)
  (add-hook 'scss-mode-hook
    (lambda ()
      ;; Treat dollar and hyphen as a word character
      (modify-syntax-entry ?$ "w")
      (modify-syntax-entry ?- "w")
      (nlinum-mode 1)
      (rainbow-mode +1))))

(add-hook 'syslog-mode-hook
  (lambda ()
    (toggle-truncate-lines +1)))

(use-package css-mode
  :mode "\\.css$"
  :config
  (use-package rainbow-mode)
  (add-hook 'css-mode-hook
    (lambda ()
      (nlinum-mode 1)
      (rainbow-mode +1))))

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(use-package markdown-mode
  :mode (("\\.markdown\\'"    . markdown-mode)
          ("\\.md\\'"    . markdown-mode))
  :config
  (use-package pandoc-mode
    :commands pandoc-mode
    :diminish pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode)

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

  (add-hook 'markdown-mode-hook
    (lambda ()
      ;; Remove for now as they interfere with indentation
      ;; (define-key yas-minor-mode-map [(tab)] nil)
      ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
      (setq imenu-generic-expression markdown-imenu-generic-expression)))

  (general-define-key :keymaps 'markdown-mode-map
    :states '(normal visual)
    :prefix grass/leader1
    "mp" 'grass/markdown-open-in-marked-app))


;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

;; Install some useful packages so this all works
;; cabal update && cabal install happy hasktags stylish-haskell present ghc-mod hlint

(use-package haskell-mode
  :defer t
  :config
  (progn

    ;; Use hi2 for indentation
    (use-package hi2
      :config
      (setq hi2-show-indentations nil)
      (add-hook 'haskell-mode-hook 'turn-on-hi2))

    (use-package ghc
      :config
      (autoload 'ghc-init "ghc" nil t)
      (autoload 'ghc-debug "ghc" nil t)
      (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

    (use-package company-ghc
      :disabled t
      :config
      (add-to-list 'company-backends 'company-ghc) (custom-set-variables '(company-ghc-show-info t)))

                                        ; Make Emacs look in Cabal directory for binaries
    (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
      (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
      (add-to-list 'exec-path my-cabal-path))

                                        ; Add F8 key combination for going to imports block
    (eval-after-load 'haskell-mode
      '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

    (setq haskell-indentation-disable-show-indentations t)

                                        ; TODO Set this up so it works properly
                                        ; Set interpreter to be "stack ghci"
    ;; (setq haskell-process-type 'ghci)
    ;; (setq haskell-process-path-ghci "stack")
    ;; (setq haskell-process-args-ghci '("ghci"))
    ;; (setq tab-always-indent t)

                                        ; Set interpreter to be "cabal repl"
                                        ;(setq haskell-process-type 'cabal-repl)

                                        ; Add key combinations for interactive haskell-mode
    ;; (eval-after-load 'haskell-mode
    ;;   '(progn
    ;;     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
    ;;     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    ;;     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    ;;     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    ;;     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    ;;     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    ;;     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
    ;;     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

    ;; (eval-after-load 'haskell-cabal
    ;;   '(progn
    ;;     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
    ;;     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    ;;     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    ;;     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    ;;     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

    ;;     ; Set interpreter to be "stack ghci"
    ;;     ;; (setq haskell-interactive-popup-errors nil)
    ;;     ;; (setq haskell-process-type 'ghci)
    ;;     ;; (setq haskell-process-path-ghci "stack")
    ;;     ;; (setq haskell-process-args-ghci '("ghci")))
    ;;   )

    ;; (eval-after-load 'haskell-mode
    ;;   '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
    ;; (eval-after-load 'haskell-cabal
    ;;   '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)))
    ))


;;;;;;;;;;
;; Lisp ;;
;;;;;;;;;;

(use-package clojure-mode
  :defer t
  :config

  (use-package flycheck-clojure
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup)))
  (add-hook 'clojure-mode-hook #'flycheck-mode)
  (add-hook 'clojure-mode-hook
    (lambda ()
      ;; Treat dash as part of a word
      (modify-syntax-entry ?- "w")
      (flycheck-mode)))

  (use-package clojure-snippets)

  (use-package cider
    :pin melpa-stable
    :init
    ;; REPL history file
    (setq cider-repl-history-file "~/.emacs.d/cider-history")

    ;; nice pretty printing
    (setq cider-repl-use-pretty-printing t)

    ;; nicer font lock in REPL
    (setq cider-repl-use-clojure-font-lock t)

    ;; result prefix for the REPL
    (setq cider-repl-result-prefix ";; => ")

    ;; never ending REPL history
    (setq cider-repl-wrap-history t)

    ;; looong history
    (setq cider-repl-history-size 3000)

    ;; error buffer not popping up
    (setq cider-show-error-buffer nil)

    ;; eldoc for clojure
    (add-hook 'cider-mode-hook #'eldoc-mode)

    ;; company mode for completion
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode))

  (use-package clj-refactor
    :pin melpa-stable
    :init
    (add-hook 'clojure-mode-hook
      (lambda ()
        (clj-refactor-mode 1)

        ;; no auto sort
        (setq cljr-auto-sort-ns nil)

        ;; do not prefer prefixes when using clean-ns
        (setq cljr-favor-prefix-notation nil)
        ;; insert keybinding setup here
        (cljr-add-keybindings-with-prefix "C-c RET")))))


(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    ;; Treat dash as part of a word
    (modify-syntax-entry ?- "w")
    (define-key global-map (kbd "C-c C-e") 'eval-print-last-sexp)))


;;;;;;;;;;;;;;;;;;;;;
;; Other Languages ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-mode
  :mode (("\\.exs?\\'"   . elixir-mode))
  :defer t
  :config
  (use-package alchemist
    :init

    ;; Hack to disable company popup in Elixir if hanging
    (eval-after-load "alchemist"
      '(defun alchemist-company--wait-for-doc-buffer ()
         (setf num 50)
         (while (and (not alchemist-company-doc-lookup-done)
                  (> (decf num) 1))
           (sit-for 0.01))))))

(use-package puppet-mode
  :defer t)

(use-package powershell
  :defer t
  :mode  (("\\.ps1$" . powershell-mode)
           ("\\.psm$" . powershell-mode)))

(use-package rust-mode
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

;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;

(use-package flyspell
  :defer t
  :commands flyspell-mode
  :diminish (flyspell-mode . " spl")
  :config
  (setq-default ispell-program-name "aspell")
  ; Silently save my personal dictionary when new items are added
  (setq ispell-silently-savep t)
  (ispell-change-dictionary "british" t)

  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
  (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

  (add-hook 'flyspell-mode-hook
    (lambda ()
      (define-key flyspell-mode-map [(control ?\,)] nil)
      (general-define-key :states '(normal visual) :prefix grass/leader1
        "S" '(:ignore t :which-key "Spelling")
        "Sn" 'flyspell-goto-next-error
        "Sw" 'ispell-word))))

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
  :disabled
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

    (require 'evil-evilified-state)
    (evilified-state-evilify-map flycheck-error-list-mode-map
      :mode flycheck-error-list-mode
      :bindings
      "RET" 'flycheck-error-list-goto-error
      "j" 'flycheck-error-list-next-error
      "k" 'flycheck-error-list-previous-error)))

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


;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(use-package general
  :init
  (general-evil-setup t)


  (general-define-key
    :states '(normal visual insert emacs)
    :prefix grass/leader1
    :non-normal-prefix "M-SPC"

    "TAB" '(grass/switch-to-previous-buffer :which-key "previous buffer")

    "c" '(:ignore t :which-key "Check/Compile")
    "cc" 'flycheck-clear
    "ch" 'flycheck-describe-checker
    "cs" 'flycheck-select-checker
    "cl" 'grass/toggle-flycheck-error-list
    "cS" 'flycheck-set-checker-executable
    "cv" 'flycheck-verify-setup

    "m" '(:ignore t :which-key "Major-mode-cmd")

    "s" '(:ignore t :which-key "Search/Replace")
    "sp" 'anzu-query-replace-at-cursor-thing
    "sa" 'counsel-ag
    "sg" 'counsel-git-grep
    "ss" 'swiper
    "s;" 'iedit-mode
    "s:" 'grass/iedit-dwim
    "sr" 'grass/replace-string-in-entire-buffer
    "sR" 'grass/replace-regexp-in-entire-buffer
    "sq" 'grass/query-replace-string-in-entire-buffer
    "sQ" 'grass/query-replace-regexp-in-entire-buffer
    "sf" 'isearch-forward-regexp
    "sb" 'isearch-reverse-regexp
    "sp" '(grass/counsel-ag-current-project :which-key "ag project")
    "sP" 'projectile-ag

    "b" '(:ignore t :which-key "Buffers")
    "bb" 'ivy-switch-buffer
    "bs" 'grass/switch-to-scratch-buffer
    "bk" 'kill-this-buffer
    "bo" 'crux-kill-other-buffers

    "k" '(:ignore t :which-key "Bookmarks")
    "ki" 'grass/open-init
    "kw" 'grass/open-work-log
    "kp" 'grass/open-personal-log
    "kn" 'grass/find-notes

    "g" '(:ignore t :which-key "Git/VC")
    "gs" 'magit-status
    "gl" 'git-link
    "gc" 'git-link-commit
    "gt" 'git-timemachine

    "e" '(:ignore t :which-key "Editing/Text")
    "eC" 'counsel-unicode-char
    "ek" 'browse-kill-ring
    "eh" 'hydra-goto-history/body
    "e~" 'hydra-change-case/body
    "ez" 'zop-up-to-char
    "ed" 'crux-indent-defun
    "ew" 'crux-cleanup-buffer-or-region
    "et" 'untabify
    "ec" 'char-menu
    "ep" 'hydra-move-parens/body

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
    "fR" 'grass/rename-file-and-buffer
    "fc" 'grass/copy-buffer-filename
    "fd" 'crux-delete-file-and-buffer
    "fj" 'dired-jump
    "fs"  '(save-buffer :which-key "save file")

    "u" '(:ignore t :which-key "Utilities")
    "uU" 'crux-view-url
    "ut" 'display-time-world
    "uc" 'quick-calc
    "uu" 'browse-url
    "ub" 'grass/comment-box
    "ud" 'grass/insert-datetime
    "uf" 'reveal-in-osx-finder

    "v" 'er/expand-region

    "w" '(:ignore t :which-key "Windows/UI")
    "wl" 'toggle-truncate-lines
    "wz" 'hydra-zoom-text/body
    "ww" 'hydra-window/body
    "wo" 'delete-other-windows
    "wk" 'delete-window
    "wt" 'crux-transpose-windows

    "z" '(:ignore t :which-key "Folding")
    "zz" 'origami-toggle-node
    "zs" 'origami-show-only-node
    "zo" 'origami-open-all-nodes
    "zu" 'origami-undo
    "zr" 'origami-redo
    )

  (general-define-key

    "s-d" 'crux-duplicate-current-line-or-region

    "<home>" 'move-beginning-of-line
    "<end>" 'move-end-of-line

    "C-x C-j" 'dired-jump
    "<s-up>" 'dired-jump

    "M-/" 'hippie-expand
    "M-z" 'zop-up-to-char
    "M-x" 'counsel-M-x
    "C-x C-f" 'counsel-find-file
    "C-c C-r" 'ivy-resume
    "<f6>" 'ivy-resume
    )

  (general-define-key :keymaps 'ivy-minibuffer-map
    "RET" 'ivy-alt-done
    "S-<up>" 'ivy-previous-history-element
    "S-<down>" 'ivy-next-history-element)

  (general-nvmap "z" 'origami-recursively-toggle-node)

  (general-imap "C-p" 'hippie-expand)

  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))


(provide 'init)
;;; init.el ends her
