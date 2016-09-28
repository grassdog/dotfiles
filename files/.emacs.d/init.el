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

;; Faster
(setq font-lock-verbose nil)

;; no jerky scrolling
(setq scroll-conservatively 101)

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

;; Wrap lines for text modes
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; Lighter line continuation arrows
(define-fringe-bitmap 'left-curly-arrow [0 64 72 68 126 4 8 0])
(define-fringe-bitmap 'right-curly-arrow [0 2 18 34 126 32 16 0])

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Make files with the same name have unique buffer names
(setq uniquify-buffer-name-style 'forward)

;; Delete selected regions
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

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

(use-package general
  :init
  ;; (setq general-default-keymaps 'evil-normal-state-map)
  (setq grass/leader1 "SPC"))

;; Must require this before spaceline
(use-package anzu
  :diminish anzu-mode
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "sr" 'anzu-query-replace-at-cursor-thing)
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
  :general
  ("C-c C-r" 'ivy-resume)
  ("<f6>" 'ivy-resume)
  (:states '(normal visual) :prefix grass/leader1
	   "bb" 'ivy-switch-buffer)
  (:keymaps 'ivy-minibuffer-map
	    "S-<up>" 'ivy-previous-history-element
	    "S-<down>" 'ivy-next-history-element)
  :init
  (use-package ivy-hydra)
  
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  ;; Don't count candidates
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

;; TODO Take more from here https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-counsel.el
(use-package counsel
  :general
  ("M-x" 'counsel-M-x)
  ("C-x C-f" 'counsel-find-file)
  (:states '(normal visual) :prefix grass/leader1
	   "fr" 'counsel-recentf
	   "ff" 'counsel-find-file
	   "fR" 'grass/rename-file-and-buffer
	   "sa" 'counsel-ag
	   "xC" 'counsel-unicode-char)
  :init
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package swiper
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "ss" 'swiper))

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
          ("grass/" . "g/")
          ("\\`hydra-" . "+h/")
          ("\\`org-babel-" . "ob/")))
  (which-key-mode 1)
  (which-key-declare-prefixes "SPC f" "files")
  (which-key-declare-prefixes "SPC x" "text")
  (which-key-declare-prefixes "SPC u" "utilities")
  (which-key-declare-prefixes "SPC b" "buffers")
  (which-key-declare-prefixes "SPC g" "git/vc")
  (which-key-declare-prefixes "SPC m" "major-mode-cmd")
  (which-key-declare-prefixes "SPC p" "projectile")
  (which-key-declare-prefixes "SPC w" "windows/ui")
  (which-key-declare-prefixes "SPC s" "search/replace"))


(use-package browse-kill-ring
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "xk" 'browse-kill-ring))


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
(general-define-key :states '(normal) :prefix grass/leader1
                    "wz" 'hydra-zoom-text/body)

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
(general-define-key :states '(normal) :prefix grass/leader1
		    "xh" 'hydra-goto-history/body)


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
    :init
    (global-evil-visualstar-mode))

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

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (setq evil-shift-width 2)

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
  (define-key evil-normal-state-map "\C-f" 'evil-forward-char)
  (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
  (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
  (define-key evil-normal-state-map "\C-b" 'evil-backward-char)
  (define-key evil-insert-state-map "\C-b" 'evil-backward-char)
  (define-key evil-visual-state-map "\C-b" 'evil-backward-char)
  (define-key evil-normal-state-map "\C-d" 'evil-delete-char)
  (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
  (define-key evil-visual-state-map "\C-d" 'evil-delete-char)
  (define-key evil-normal-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-visual-state-map "\C-n" 'evil-next-line)
  (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
  (define-key evil-normal-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-visual-state-map "\C-y" 'yank)
  (define-key evil-normal-state-map "\C-k" 'kill-line)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-visual-state-map "\C-k" 'kill-line)
  (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
  (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)

  ;; Set our default modes
  ;; TODO Look to spacemacs for robust settings here
  (loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                                (nrepl-mode . insert)
                                (pylookup-mode . emacs)
                                (comint-mode . normal)
                                (shell-mode . emacs)
                                (git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (calculator-mode . emacs)
                                (term-mode . emacs)
                                (haskell-interactive-mode . emacs)
                                (undo-tree-visualizer-mode . emacs)
                                (cider-repl-mode . emacs)
                                (help-mode . normal)
                                (grep-mode . emacs)
                                (bc-menu-mode . emacs)
                                (erc-mode . emacs)
                                (magit-branch-manager-mode . emacs)
                                (magit-blame-mode-map . emacs)
                                (magit-cherry-mode-map . emacs)
                                (magit-diff-mode-map . emacs)
                                (magit-log-mode-map . emacs)
                                (magit-log-select-mode-map . emacs)
                                (magit-mode-map . emacs)
                                (magit-popup-help-mode-map . emacs)
                                (magit-popup-mode-map . emacs)
                                (magit-popup-sequence-mode-map . emacs)
                                (magit-process-mode-map . emacs)
                                (magit-reflog-mode-map . emacs)
                                (magit-refs-mode-map . emacs)
                                (magit-revision-mode-map . emacs)
                                (magit-stash-mode-map . emacs)
                                (magit-stashes-mode-map . emacs)
                                (magit-status-mode-map . emacs)
                                (rdictcc-buffer-mode . emacs)
				(kill-ring-mode . normal)
                                (bs-mode . emacs)
                                (dired-mode . normal)
                                (wdired-mode . normal))
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

; TODO Fix this move text business
(use-package move-text
  :commands (move-text-up move-text-down)
  :bind (("<C-S-up>" . move-text-up)
         ("<C-S-down>" . move-text-down)))

(defhydra hydra-move-text ()
  "move text"
  ("u" move-text-up "move up")
  ("d" move-text-down "move down"))
(general-define-key :states '(normal) :prefix grass/leader1 "xm" 'hydra-move-text/body)

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

(defhydra hydra-case ()
  "toggle word case"
  ("c" capitalize-word "Capitalize")
  ("u" upcase-word "UPPER")
  ("l" downcase-word "lower")
  ("s" string-inflection-underscore "lower_snake")
  ("n" string-inflection-upcase "UPPER_SNAKE")
  ("a" string-inflection-lower-camelcase "lowerCamel")
  ("m" string-inflection-camelcase "UpperCamel")
  ("d" string-inflection-lisp "dash-case"))
(general-define-key :states '(normal) :prefix grass/leader1 "x~" 'hydra-case/body)

;; Better zap to char
(use-package zop-to-char
  :bind ("M-z" . zop-up-to-char)
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "xz" 'zop-up-to-char)
  :commands (zop-to-char zop-up-to-char))

(global-set-key [remap zap-to-char] 'zop-to-char)

(use-package iedit
  :defines grass/iedit-dwim
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "s;" 'iedit-mode
	   "s:" 'grass/iedit-dwim)
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

(general-define-key :states '(normal visual) :prefix grass/leader1
		    "ww" 'hydra-window/body
		    "wk" 'delete-window)


;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(general-define-key :states '(normal visual) :prefix grass/leader1
		    "ut" 'display-time-world
		    "uc" 'quick-calc
		    "uu" 'browse-url
		    "ub" 'grass/comment-box)

(use-package reveal-in-osx-finder
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "uf" 'reveal-in-osx-finder))


;;;;;;;;;
;; Git ;;
;;;;;;;;;

(use-package magit
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "gs" 'magit-status))

(use-package git-link
  :config
  (setq git-link-open-in-browser t)
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "gl" 'git-link
	   "gc" 'git-link-commit))

(use-package git-timemachine
  :commands git-timemachine
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "gt" 'git-timemachine))

(use-package git-gutter
  :commands global-git-gutter-mode
  :diminish git-gutter-mode
  :defer 3
  :config
  (progn
    ;; If you enable global minor mode
    (global-git-gutter-mode t)
    ;; If you would like to use git-gutter.el and linum-mode
    (git-gutter:linum-setup)
    (setq git-gutter:update-interval 2
          git-gutter:modified-sign " "
          git-gutter:added-sign "+"
          git-gutter:deleted-sign "-"
          git-gutter:diff-option "-w"
          git-gutter:hide-gutter t
          git-gutter:ask-p nil
          git-gutter:verbosity 0
          git-gutter:handled-backends '(git hg bzr svn)
          git-gutter:hide-gutter t)))

(use-package git-gutter-fringe
  :commands git-gutter-mode
  :defer 3
  :config
  (progn
    (when (display-graphic-p)
      (with-eval-after-load 'git-gutter
        (require 'git-gutter-fringe)))
    (setq git-gutter-fr:side 'right-fringe))
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
                          ))


;;;;;;;;;;;;;;;;;;;;;;
;; Symbol insertion ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package char-menu
  :commands char-menu
  ; Em-dash is first
  :config (setq char-menu '("—" "‘’" "“”" "…" "«»" "–"
                            ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                            ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
                            ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓"))))

(general-define-key :states '(normal visual) :prefix grass/leader1
		    "xc" 'char-menu)


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

  (use-package dired-single
    :commands 'dired-single-buffer)

  (use-package peep-dired
    :general
    (dired-mode-map :states :prefix grass/leader1
			    "mp" 'peep-dired))

  (diminish 'dired-omit-mode "")
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

;; TODO Somehow mute the colours in dired
(use-package dired+
  :general
  ("C-x C-j" 'dired-jump
   "<s-up>" 'dired-jump))


;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Search and Replace ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; http://sachachua.com/blog/2008/07/emacs-keyboard-shortcuts-for-navigating-code/
;; (defun grass/isearch-yank-current-word ()
;;   "Pull current word from buffer into search string."
;;   (interactive)
;;   (save-excursion
;;     (skip-syntax-backward "w_")
;;     (isearch-yank-internal
;;      (lambda ()
;;        (skip-syntax-forward "w_")
;;        (point)))))

;; (define-key isearch-mode-map (kbd "C-x") 'grass/isearch-yank-current-word)

;; (defun grass/search-word-backward ()
;;   "Find the previous occurrence of the current word."
;;   (interactive)
;;   (let ((cur (point)))
;;     (skip-syntax-backward "w_")
;;     (goto-char
;;      (if (re-search-backward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t)
;;    (match-beginning 0)
;;        cur))))

;; (defun grass/search-word-forward ()
;;   "Find the next occurrance of the current word."
;;   (interactive)
;;   (let ((cur (point)))
;;     (skip-syntax-forward "w_")
;;     (goto-char
;;      (if (re-search-forward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t)
;;    (match-beginning 0)
;;        cur))))
;; (global-set-key '[M-up] 'grass/search-word-backward)
;; (global-set-key '[M-down] 'grass/search-word-forward)


;; (defun grass/replace-string (from-string to-string &optional delimited start end)
;;   "This is a modified version of `replace-string'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
;;   (interactive
;;    (let ((common
;;           (query-replace-read-args
;;            (concat "Replace"
;;                    (if current-prefix-arg " word" "")
;;                    (if (and transient-mark-mode mark-active) " in region" ""))
;;            nil)))
;;      (list (nth 0 common) (nth 1 common) (nth 2 common)
;;            (if (and transient-mark-mode mark-active)
;;                (region-beginning)
;;              (buffer-end -1))
;;            (if (and transient-mark-mode mark-active)
;;                (region-end)
;;              (buffer-end 1)))))
;;   (perform-replace from-string to-string nil nil delimited nil nil start end))

;; (defun grass/replace-regexp (regexp to-string &optional delimited start end)
;;   "This is a modified version of `replace-regexp'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
;;   (interactive
;;    (let ((common
;;           (query-replace-read-args
;;            (concat "Replace"
;;                    (if current-prefix-arg " word" "")
;;                    " regexp"
;;                    (if (and transient-mark-mode mark-active) " in region" ""))
;;            t)))
;;      (list (nth 0 common) (nth 1 common) (nth 2 common)
;;            (if (and transient-mark-mode mark-active)
;;                (region-beginning)
;;              (buffer-end -1))
;;            (if (and transient-mark-mode mark-active)
;;                (region-end)
;;              (buffer-end 1)))))
;;   (perform-replace regexp to-string nil t delimited nil nil start end))

;; (defun grass/query-replace-regexp (regexp to-string &optional delimited start end)
;;   "This is a modified version of `query-replace-regexp'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
;;   (interactive
;;    (let ((common
;;           (query-replace-read-args
;;            (concat "Replace"
;;                    (if current-prefix-arg " word" "")
;;                    " regexp"
;;                    (if (and transient-mark-mode mark-active) " in region" ""))
;;            t)))
;;      (list (nth 0 common) (nth 1 common) (nth 2 common)
;;            (if (and transient-mark-mode mark-active)
;;                (region-beginning)
;;              (buffer-end -1))
;;            (if (and transient-mark-mode mark-active)
;;                (region-end)
;;              (buffer-end 1)))))
;;   (perform-replace regexp to-string t t delimited nil nil start end))

;; (defun grass/query-replace-string (from-string to-string &optional delimited start end)
;;   "This is a modified version of `query-replace-string'. This modified version defaults to operating on the entire buffer instead of working only from POINT to the end of the buffer."
;;   (interactive
;;    (let ((common
;;           (query-replace-read-args
;;            (concat "Replace"
;;                    (if current-prefix-arg " word" "")
;;                    (if (and transient-mark-mode mark-active) " in region" ""))
;;            nil)))
;;      (list (nth 0 common) (nth 1 common) (nth 2 common)
;;            (if (and transient-mark-mode mark-active)
;;                (region-beginning)
;;              (buffer-end -1))
;;            (if (and transient-mark-mode mark-active)
;;                (region-end)
;;              (buffer-end 1)))))
;;   (perform-replace from-string to-string t nil delimited nil nil start end))

;; (which-key-declare-prefixes "C-, s" "search/replace")
;; (global-set-key (kbd "C-, s r") 'grass/replace-string)
;; (global-set-key (kbd "C-, s R") 'grass/replace-regexp)
;; (global-set-key (kbd "C-, s q") 'grass/query-replace-string)
;; (global-set-key (kbd "C-, s Q") 'grass/query-replace-regexp)
;; (global-set-key (kbd "C-, s f") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-, s b") 'isearch-reverse-regexp)

;; (use-package ag
;;   :bind (("C-, s a" . ag-project))
;;   :commands ag-project)

;; (use-package anzu
;;   :diminish anzu-mode
;;   :defer 3
;;   :config
;;   (setq anzu-cons-mode-line-p nil)
;;   (global-anzu-mode +1))


;; ;;;;;;;;;;;;;;;
;; ;; Selection ;;
;; ;;;;;;;;;;;;;;;

;; (use-package expand-region
;;   :bind (("C-+" . er/contract-region)
;;          ("C-=" . er/expand-region)
;;          ("s-e" . er/expand-region)
;;          ("s-E" . er/contract-region)))

;; (defun grass/mark-full-line ()
;;   "Set mark from point to beginning of line"
;;   (interactive)
;;   (beginning-of-line)
;;   (call-interactively 'set-mark-command)
;;   (end-of-line))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Autocomplete and snippets ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; abbrev mode for common typos
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; (diminish 'abbrev-mode "ⓐ")
;; (setq-default abbrev-mode t)

;; (use-package company
;;   :diminish (company-mode . "ⓒ")
;;   :config
;;   (setq company-idle-delay 0.2)
;;   (setq company-minimum-prefix-length 3)
;;   (setq company-dabbrev-ignore-case nil)
;;   (setq company-dabbrev-downcase nil)
;;   (setq company-global-modes
;;         '(not markdown-mode org-mode erc-mode))

;;   (define-key company-active-map [escape] 'company-abort))

;; (add-hook 'after-init-hook 'global-company-mode)

;; (use-package yasnippet
;;   :diminish (yas-minor-mode . "ⓨ")
;;   :defer 1
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   (defun grass/do-yas-expand ()
;;     (let ((yas/fallback-behavior 'return-nil))
;;       (yas-expand)))

;;   (defun grass/check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))

;;   (defun grass/tab-indent-or-complete ()
;;     (interactive)
;;     (cond
;;      ((minibufferp)
;;       (minibuffer-complete))
;;      (t
;;       (if (or (not yas-minor-mode)
;;               (null (grass/do-yas-expand)))
;;           (if (grass/check-expansion)
;;               (progn
;;                 (company-manual-begin)
;;                 (if (null company-candidates)
;;                     (progn
;;                       (company-abort)
;;                       (indent-for-tab-command))))
;;             (indent-for-tab-command))
;;         (indent-for-tab-command)))))

;;   (defun grass/tab-complete-or-next-field ()
;;     (interactive)
;;     (if (or (not yas-minor-mode)
;;             (null (grass/do-yas-expand)))
;;         (if company-candidates
;;             (company-complete-selection)
;;           (if (grass/check-expansion)
;;               (progn
;;                 (company-manual-begin)
;;                 (if (null company-candidates)
;;                     (progn
;;                       (company-abort)
;;                       (yas-next-field))))
;;             (yas-next-field)))))

;;   (defun grass/expand-snippet-or-complete-selection ()
;;     (interactive)
;;     (if (or (not yas-minor-mode)
;;             (null (grass/do-yas-expand))
;;             (company-abort))
;;         (company-complete-selection)))

;;   (defun grass/abort-company-or-yas ()
;;     (interactive)
;;     (if (null company-candidates)
;;         (yas-abort-snippet)
;;       (company-abort)))

;;   (setq yas-verbosity 1)
;;   (yas-global-mode 1)

;;   (global-set-key [tab] 'grass/tab-indent-or-complete)
;;   (global-set-key (kbd "TAB") 'grass/tab-indent-or-complete)
;;   (global-set-key [(control return)] 'company-complete-common)

;;   (define-key company-active-map [tab] 'grass/expand-snippet-or-complete-selection)
;;   (define-key company-active-map (kbd "TAB") 'grass/expand-snippet-or-complete-selection)

;;   (define-key yas-minor-mode-map [tab] nil)
;;   (define-key yas-minor-mode-map (kbd "TAB") nil)

;;   ;; Don't enable smartparens when expanding
;;   (defvar smartparens-enabled-initially t
;;     "Whether smartparens is originally enabled or not.")

;;   (add-hook 'yas-before-expand-snippet-hook (lambda ()
;;                                               ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
;;                                               (setq smartparens-enabled-initially smartparens-mode)
;;                                               (smartparens-mode -1)))
;;   (add-hook 'yas-after-exit-snippet-hook (lambda ()
;;                                            (when smartparens-enabled-initially
;;                                              (smartparens-mode 1))))

;;   (define-key yas-keymap [tab] 'grass/tab-complete-or-next-field)
;;   (define-key yas-keymap (kbd "TAB") 'grass/tab-complete-or-next-field)
;;   (define-key yas-keymap [(control tab)] 'yas-next-field)
;;   (define-key yas-keymap (kbd "C-g") 'grass/abort-company-or-yas)
;;   (define-key yas-minor-mode-map (kbd "C-, e") 'yas-expand))


;; ;;;;;;;;;;;;;;
;; ;; Wrapping ;;
;; ;;;;;;;;;;;;;;

;; (use-package smartparens
;;   :diminish (smartparens-mode . "ⓢ")
;;   :commands (sp-unwrap-sexp sp-rewrap-sexp)
;;   :config
;;   (require 'smartparens-config)
;;   (sp-use-smartparens-bindings)

;;   ;; Wrap an entire symbol
;;   (setq sp-wrap-entire-symbol nil)

;;   ;; No auto pairing of quotes thanks
;;   (sp-pair "'" nil :actions '(:rem insert))
;;   (sp-pair "\"" nil :actions '(:rem insert))

;;   (progn
;;     (defun my-elixir-do-end-close-action (id action context)
;;       (when (eq action 'insert)
;;         (newline-and-indent)
;;         (previous-line)
;;         (indent-according-to-mode)))

;;     (sp-with-modes '(elixir-mode)
;;       (sp-local-pair "do" "end"
;;                      :when '(("SPC" "RET"))
;;                      :post-handlers '(:add my-elixir-do-end-close-action)
;;                      :actions '(insert))

;;       (sp-local-pair "fn" "end"
;;                      :when '(("SPC" "RET"))
;;                      :post-handlers '(:add my-elixir-do-end-close-action)
;;                      :actions '(insert)))))

;; (add-hook 'prog-mode-hook #'smartparens-mode)


;; (use-package corral
;;   :commands (corral-parentheses-backward
;;              corral-parentheses-forward
;;              corral-brackets-backward
;;              corral-brackets-forward
;;              corral-braces-backward
;;              corral-braces-forward))

;; (defhydra hydra-corral (:columns 4)
;;   "Corral"
;;   ("r" sp-rewrap-sexp "Rewrap" :exit t)
;;   ("u" sp-unwrap-sexp "Unwrap")
;;   ("(" corral-parentheses-backward "Back")
;;   (")" corral-parentheses-forward "Forward")
;;   ("[" corral-brackets-backward "Back")
;;   ("]" corral-brackets-forward "Forward")
;;   ("{" corral-braces-backward "Back")
;;   ("}" corral-braces-forward "Forward")
;;   ("." hydra-repeat "Repeat"))
;; (global-set-key (kbd "C-, '") #'hydra-corral/body)


;; ;;;;;;;;;;;;;;;
;; ;; Alignment ;;
;; ;;;;;;;;;;;;;;;

;; ;; Modified function from http://emacswiki.org/emacs/AlignCommands
;; (defun align-repeat (start end regexp &optional justify-right after)
;;   "Repeat alignment with respect to the given regular expression.
;; If JUSTIFY-RIGHT is non nil justify to the right instead of the
;; left. If AFTER is non-nil, add whitespace to the left instead of
;; the right."
;;   (interactive "r\nsAlign regexp: ")
;;   (let ((complete-regexp (if after
;;                              (concat regexp "\\([ \t]*\\)")
;;                            (concat "\\([ \t]*\\)" regexp)))
;;         (group (if justify-right -1 1)))
;;     (align-regexp start end complete-regexp group 1 t)))

;; ;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
;; (defun align-repeat-decimal (start end)
;;   "Align a table of numbers on decimal points and dollar signs (both optional)"
;;   (interactive "r")
;;   (require 'align)
;;   (align-region start end nil
;;                 '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
;;                        (repeat . t)
;;                        (group 1 2)
;;                        (spacing 1 1)
;;                        (justify nil t)))
;;                 nil))

;; (defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
;;   (let ((new-func (intern (concat "align-repeat-" name))))
;;     `(defun ,new-func (start end switch)
;;        (interactive "r\nP")
;;        (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
;;          (align-repeat start end ,regexp ,justify-right after)))))

;; (create-align-repeat-x "comma" "," nil t)
;; (create-align-repeat-x "semicolon" ";" nil t)
;; (create-align-repeat-x "colon" ":" nil t)
;; (create-align-repeat-x "equal" "=")
;; (create-align-repeat-x "hash" "=>")
;; (create-align-repeat-x "math-oper" "[+\\-*/]")
;; (create-align-repeat-x "ampersand" "&")
;; (create-align-repeat-x "bar" "|")
;; (create-align-repeat-x "left-paren" "(")
;; (create-align-repeat-x "right-paren" ")" t)

;; ;; Bindings
;; (which-key-declare-prefixes "C-, a" "alignment")
;; (global-set-key (kbd "C-, a a") 'align)
;; (global-set-key (kbd "C-, a r") 'align-repeat)
;; (global-set-key (kbd "C-, a m") 'align-repeat-math-oper)
;; (global-set-key (kbd "C-, a .") 'align-repeat-decimal)
;; (global-set-key (kbd "C-, a ,") 'align-repeat-comma)
;; (global-set-key (kbd "C-, a ;") 'align-repeat-semicolon)
;; (global-set-key (kbd "C-, a :") 'align-repeat-colon)
;; (global-set-key (kbd "C-, a =") 'align-repeat-equal)
;; (global-set-key (kbd "C-, a >") 'align-repeat-hash)
;; (global-set-key (kbd "C-, a &") 'align-repeat-ampersand)
;; (global-set-key (kbd "C-, a |") 'align-repeat-bar)
;; (global-set-key (kbd "C-, a (") 'align-repeat-left-paren)
;; (global-set-key (kbd "C-, a )") 'align-repeat-right-paren)


;; ;;;;;;;;;;;;;;;
;; ;; Prog mode ;;
;; ;;;;;;;;;;;;;;;

;; (use-package nlinum
;;   :load-path "vendor"
;;   :commands nlinum-mode
;;   :preface
;;   (setq nlinum-format "%4d "))

;; (use-package rainbow-delimiters
;;   :commands rainbow-delimiters-mode)

;; ;; Line numbers for coding please
;; (add-hook 'prog-mode-hook
;;             (lambda ()
;;               ;; Treat underscore as a word character
;;               (modify-syntax-entry ?_ "w")
;;               (nlinum-mode 1)
;;               (rainbow-delimiters-mode)))


;; ;;;;;;;;;;;;;;;;;
;; ;; Indentation ;;
;; ;;;;;;;;;;;;;;;;;

;; ;; Simple indentation please
;; (use-package clean-aindent-mode
;;   :disabled
;;   :init
;;   ; no electric indent, auto-indent is sufficient
;;   (electric-indent-mode -1)
;;   (clean-aindent-mode t)
;;   (setq clean-aindent-is-simple-indent t))

;; ;; Don't use tabs to indent
;; (setq-default indent-tabs-mode nil)

;; ;; Default indentation
;; (setq-default tab-width 2)

;; ;; Javascript
;; (setq-default js2-basic-offset 2)

;; ;; JSON
;; (setq-default js-indent-level 2)

;; ;; Sass
;; (setq css-indent-offset 2)

;; ;; Coffeescript
;; (setq coffee-tab-width 2)

;; ;; Python
;; (setq-default py-indent-offset 2)

;; ;; XML
;; (setq-default nxml-child-indent 2)

;; ;; Ruby
;; (setq ruby-indent-level 2)

;; ;; Default formatting style for C based modes
;; (setq c-default-style "java")
;; (setq-default c-basic-offset 2)

;; ; https://gist.github.com/mishoo/5487564
;; (defcustom stupid-indent-level 2
;;   "Indentation level for stupid-indent-mode")

;; (defun stupid-outdent-line ()
;;   (interactive)
;;   (let (col)
;;     (save-excursion
;;       (beginning-of-line-text)
;;       (setq col (- (current-column) stupid-indent-level))
;;       (when (>= col 0)
;;         (indent-line-to col)))))

;; (defun stupid-outdent-region (start stop)
;;   (interactive)
;;   (setq stop (copy-marker stop))
;;   (goto-char start)
;;   (while (< (point) stop)
;;     (unless (and (bolp) (eolp))
;;       (stupid-outdent-line))
;;     (forward-line 1)))

;; (defun stupid-outdent ()
;;   (interactive)
;;   (if (use-region-p)
;;       (save-excursion
;;         (stupid-outdent-region (region-beginning) (region-end))
;;         (setq deactivate-mark nil))
;;     (stupid-outdent-line)))

;; (global-set-key (kbd "<backtab>") 'stupid-outdent)


;; ;;;;;;;;;;;;;;;;
;; ;; Whitespace ;;
;; ;;;;;;;;;;;;;;;;

;; (require 'whitespace)
;; (diminish 'global-whitespace-mode)

;; (setq require-final-newline t)

;; (define-key global-map (kbd "C-, f w") 'whitespace-cleanup)

;; ;; Only show bad whitespace (Ignore empty lines at start and end of buffer)
;; (setq whitespace-style '(face tabs trailing space-before-tab indentation space-after-tab))
;; (global-whitespace-mode t)

;; ;; Only trim modified lines on save
;; (use-package ws-butler
;;   :diminish (ws-butler-mode . "ⓦ")
;;   :config
;;   (progn
;;     (ws-butler-global-mode 1)))


;; ;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Utility Functions ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; (defun grass/today ()
;;   (format-time-string "%Y-%m-%d, %a"))

;; (defun grass/insert-date ()
;;   (interactive)
;;   (insert (grass/today)))

;; (defun grass/view-url-in-buffer ()
;;   "Open a new buffer containing the contents of URL."
;;   (interactive)
;;   (let* ((default (thing-at-point-url-at-point))
;;          (url (read-from-minibuffer "URL: " default)))
;;     (switch-to-buffer (url-retrieve-synchronously url))
;;     (rename-buffer url t)
;;     (cond ((search-forward "<?xml" nil t) (xml-mode))
;;           ((search-forward "<html" nil t) (html-mode)))))

;; (defun grass/indent-buffer ()
;;   "Indents the entire buffer."
;;   (indent-region (point-min) (point-max)))

;; (defun grass/indent-region-or-buffer ()
;;   "Indents a region if selected, otherwise the whole buffer."
;;   (interactive)
;;   (save-excursion
;;     (if (region-active-p)
;;         (progn
;;           (indent-region (region-beginning) (region-end))
;;           (message "Indented selected region."))
;;       (progn
;;         (grass/indent-buffer)
;;         (message "Indented buffer.")))))

;; Quick buffer switch
(defun grass/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; (defun grass/rename-file-and-buffer ()
;;   "Renames current buffer and file it is visiting."
;;   (interactive)
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not (and filename (file-exists-p filename)))
;;         (error "Buffer '%s' is not visiting a file!" name)
;;       (let ((new-name (read-file-name "New name: " filename)))
;;         (cond ((get-buffer new-name)
;;                (error "A buffer named '%s' already exists!" new-name))
;;               (t
;;                (rename-file filename new-name 1)
;;                (rename-buffer new-name)
;;                (set-visited-file-name new-name)
;;                (set-buffer-modified-p nil)
;;                (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; (defun grass/what-face (pos)
;;   "Identify the face under point"
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;;                   (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; (defun grass/killsave-to-end-of-line ()
;;   "Kill save till end of line from point"
;;   (interactive)
;;   (kill-append (buffer-substring-no-properties (point) (line-end-position)) nil))

;; ;;;;;;;;;;;;;;;;;;
;; ;; Common Files ;;
;; ;;;;;;;;;;;;;;;;;;

;; (defun grass/open-cheats ()
;;   "Open Emacs cheats file"
;;   (interactive)
;;   (find-file "~/Dropbox/Notes/Emacs.md"))

;; (defun grass/open-work-log ()
;;   "Open Worklog file"
;;   (interactive)
;;   (find-file "~/Dropbox/Notes/Work Log.org"))

;; (defun grass/open-sideproject-log ()
;;   "Open Worklog file"
;;   (interactive)
;;   (find-file "~/Dropbox/Notes/Sideproject Log.org"))

;; (defun grass/find-notes ()
;;   "Find a note in Dropbox/Notes directory"
;;   (interactive)
;;   (helm-browse-project-find-files (expand-file-name "~/Dropbox/Notes")))

;; (which-key-declare-prefixes "C-, b" "bookmarks")
;; (global-set-key (kbd "C-, b c") 'grass/open-cheats)
;; (global-set-key (kbd "C-, b w") 'grass/open-work-log)
;; (global-set-key (kbd "C-, b s") 'grass/open-sideproject-log)
;; (global-set-key (kbd "C-, b n") 'grass/find-notes)

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(use-package projectile
  :diminish (projectile-mode . "ⓟ")
  :commands projectile-mode
  :general
  (:states '(normal visual) :prefix grass/leader1
	   "p" '(:keymap projectile-command-map))
  :config
  (setq projectile-tags-command "rtags")
  (setq projectile-enable-caching nil)
  (setq projectile-completion-system 'ivy)
  ;; Show unadded files also
  (setq projectile-hg-command "( hg locate -0 -I . ; hg st -u -n -0 )")

  (add-to-list 'projectile-globally-ignored-directories "gems")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "bower_components")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "/emacs.d/elpa/")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")

  (add-to-list 'projectile-globally-ignored-files ".keep")
  (add-to-list 'projectile-globally-ignored-files "TAGS")

  (projectile-global-mode t))

;; ;;;;;;;;;
;; ;; Org ;;
;; ;;;;;;;;;

;; (use-package org
;;   :defer t
;;   :bind ("C-c a" . org-agenda)

;;   :config
;;   ;; Make windmove work in org-mode
;;   (setq org-replace-disputed-keys t)
;;   (setq org-return-follows-link t)
;;   ;; Show indents
;;   (setq org-startup-indented t)
;;   (setq org-hide-leading-stars t)
;;   (setq org-agenda-files '("~/Dropbox/Notes"))
;;   ;; prevent demoting heading also shifting text inside sections
;;   (setq org-adapt-indentation nil)

;;   ;; Use pandoc for exports
;;   (use-package ox-pandoc)

;;   ;; Create reveal js presentations in org mode.
;;   (use-package ox-reveal
;;     :init
;;     (setq org-reveal-root (concat "file://" (expand-file-name "~/Dropbox/Backups/Reveal/reveal.js")))
;;     ;; Use htmlize to highlight source code block using my emacs theme
;;     (use-package htmlize))

;;   (use-package org-mac-link
;;     :bind ("C-c g" . org-mac-grab-link))

;;   ;; Show raw link text
;;   (setq org-descriptive-links nil)
;;   ;; Start up fully open
;;   (setq org-startup-folded nil)

;;   (defun org-summary-todo (n-done n-not-done)
;;     "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;     (let (org-log-done org-log-states)   ; turn off logging
;;       (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;   (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;   (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))

;;   ;; Allow bind in files to enable export overrides
;;   (setq org-export-allow-bind-keywords t)
;;   (defun grass/html-filter-remove-src-blocks (text backend info)
;;     "Remove source blocks from html export."
;;     (when (org-export-derived-backend-p backend 'html) ""))

;;   ;; Code blocks
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (js . t)
;;      (ruby . t)
;;      (sh . t)))

;;   ;; Highlight source blocks
;;   (setq org-src-fontify-natively t
;;         org-src-tab-acts-natively t
;;         org-confirm-babel-evaluate nil)

;;   (add-hook 'org-shiftup-final-hook 'windmove-up)
;;   (add-hook 'org-shiftleft-final-hook 'windmove-left)
;;   (add-hook 'org-shiftdown-final-hook 'windmove-down)
;;   (add-hook 'org-shiftright-final-hook 'windmove-right)
;;   (defhydra hydra-org (:color red :columns 3)
;;     "Org Mode Movements"
;;     ("n" outline-next-visible-heading "next heading")
;;     ("p" outline-previous-visible-heading "prev heading")
;;     ("N" org-forward-heading-same-level "next heading at same level")
;;     ("P" org-backward-heading-same-level "prev heading at same level")
;;     ("u" outline-up-heading "up heading")
;;     ("g" org-goto "goto" :exit t))

;;   (add-hook 'org-mode-hook
;;     (lambda ()
;;       ;; No auto indent please
;;       (setq org-export-html-postamble nil)
;;       ;; Let me keep my prefix key binding
;;       (define-key org-mode-map (kbd "C-,") nil)
;;       ;; (org-hide-block-all)
;;       ;; (define-key org-mode-map (kbd "C-c t") 'org-hide-block-toggle)
;;       (define-key org-mode-map (kbd "C-, g h") 'hydra-org/body)
;;       (define-key org-mode-map (kbd "C-, a") 'org-cycle-agenda-files))))

;; ;;;;;;;;;;
;; ;; Ruby ;;
;; ;;;;;;;;;;

;; (use-package enh-ruby-mode
;;   :mode (("\\.rb$"        . enh-ruby-mode)
;;          ("\\.ru$"        . enh-ruby-mode)
;;          ("\\.rake$"      . enh-ruby-mode)
;;          ("\\.gemspec$"   . enh-ruby-mode)
;;          ("\\.?pryrc$"    . enh-ruby-mode)
;;          ("/Gemfile$"     . enh-ruby-mode)
;;          ("/Guardfile$"   . enh-ruby-mode)
;;          ("/Capfile$"     . enh-ruby-mode)
;;          ("/Vagrantfile$" . enh-ruby-mode)
;;          ("/Rakefile$"    . enh-ruby-mode))
;;   :interpreter "ruby"
;;   :config

;;   (use-package inf-ruby
;;     :config
;;     (setq inf-ruby-default-implementation "pry")
;;     (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

;;   (use-package rspec-mode)

;;   ;; We never want to edit Rubinius bytecode
;;   (add-to-list 'completion-ignored-extensions ".rbc")

;;   (add-hook 'enh-ruby-mode-hook
;;     (lambda ()
;;       ;; turn off the annoying input echo in irb
;;       (setq comint-process-echoes t)

;;       ;; Indentation
;;       (setq ruby-indent-level 2)
;;       (setq ruby-deep-indent-paren nil)
;;       (setq enh-ruby-bounce-deep-indent t)
;;       (setq enh-ruby-hanging-brace-indent-level 2)
;;       (setq enh-ruby-indent-level 2)
;;       (setq enh-ruby-deep-indent-paren nil)

;;       ;; Abbrev mode seems broken for some reason
;;       (abbrev-mode -1))))


;; (use-package chruby
;;   :commands chruby-use-corresponding)

;; (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding)


;; ;;;;;;;;;;;;;;;;
;; ;; Javascript ;;
;; ;;;;;;;;;;;;;;;;

;; (use-package js2-mode
;;   :mode  (("\\.js$" . js2-jsx-mode)
;;           ("\\.jsx?$" . js2-jsx-mode)
;;           ("\\.es6$" . js2-mode))
;;   :interpreter "node"
;;   :config
;;     (use-package js2-refactor
;;       :init
;;       (add-hook 'js2-mode-hook #'js2-refactor-mode)
;;       (js2r-add-keybindings-with-prefix "C-c RET"))

;;     ;; Rely on flycheck instead...
;;     (setq js2-show-parse-errors nil)
;;     ;; Reduce the noise
;;     (setq js2-strict-missing-semi-warning nil)
;;     ;; jshint does not warn about this now for some reason
;;     (setq js2-strict-trailing-comma-warning nil)

;;     (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;;     (add-hook 'js2-mode-hook
;;       (lambda ()
;;         (setq mode-name "JS2")
;;         (setq js2-global-externs '("module" "require" "buster" "jestsinon" "jasmine" "assert"
;;                                   "it" "expect" "describe" "beforeEach"
;;                                   "refute" "setTimeout" "clearTimeout" "setInterval"
;;                                   "clearInterval" "location" "__dirname" "console" "JSON"))

;;         (flycheck-mode 1)
;;         (js2-imenu-extras-mode +1))))

;; (use-package json-mode
;;   :mode "\\.json$"
;;   :bind ("C-, u j" . json-pretty-print-buffer)
;;   :config
;;   (use-package flymake-json
;;     :init
;;     (add-hook 'json-mode 'flymake-json-load))

;;   (flycheck-mode 1))

;; (use-package typescript-mode
;;   :mode "\\.ts$"
;;   :config
;;   (setq typescript-indent-level 2)
;;   (setq typescript-expr-indent-offset 2)
;;   (use-package tss
;;     :init
;;     (setq tss-popup-help-key "C-, , h")
;;     (setq tss-jump-to-definition-key "C-, , j")
;;     (setq tss-implement-definition-key "C-, , i")
;;     (tss-config-default)))

;; (use-package elm-mode
;;   :mode "\\.elm$"
;;   :config

;;   (use-package flycheck-elm
;;     :init
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))


;;   (add-hook 'elm-mode-hook
;;     (lambda ()
;;       ;; Reenable elm oracle once it's start up cost doesn't smash editor performance
;;       ;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

;;       (setq tab-width 4)
;;       (flycheck-mode t))))


;; ;;;;;;;;;;;;
;; ;; Coffee ;;
;; ;;;;;;;;;;;;

;; (defun grass/indent-relative (&optional arg)
;;   "Newline and indent same number of spaces as previous line."
;;   (interactive)
;;   (let* ((indent (+ 0 (save-excursion
;;                         (back-to-indentation)
;;                         (current-column)))))
;;     (newline 1)
;;     (insert (make-string indent ?\s))))

;; (use-package coffee-mode
;;   :mode  "\\.coffee$"
;;   :config
;;   (progn
;;     ;; Proper indents when we evil-open-below etc...
;;     (defun grass/coffee-indent ()
;;       (if (coffee-line-wants-indent)
;;           ;; We need to insert an additional tab because the last line was special.
;;           (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
;;         ;; Otherwise keep at the same indentation level
;;         (coffee-insert-spaces (coffee-previous-indent))))

;;     ;; Override indent for coffee so we start at the same indent level
;;     (defun grass/coffee-indent-line ()
;;       "Indent current line as CoffeeScript."
;;       (interactive)
;;       (let* ((curindent (current-indentation))
;;              (limit (+ (line-beginning-position) curindent))
;;              (type (coffee--block-type))
;;              indent-size
;;              begin-indents)
;;         (if (and type (setq begin-indents (coffee--find-indents type limit '<)))
;;             (setq indent-size (coffee--decide-indent curindent begin-indents '>))
;;           (let ((prev-indent (coffee-previous-indent))
;;                 (next-indent-size (+ curindent coffee-tab-width)))
;;             (if (= curindent 0)
;;                 (setq indent-size prev-indent)
;;               (setq indent-size (+ curindent coffee-tab-width) ))
;;             (coffee--indent-insert-spaces indent-size)))))

;;     (add-hook 'coffee-mode-hook
;;               (lambda ()
;;                 (set (make-local-variable 'tab-width) 2)
;;                 (flycheck-mode t)
;;                 (setq indent-line-function 'grass/coffee-indent-line)))))
;; ;;;;;;;;;
;; ;; Web ;;
;; ;;;;;;;;;

;; (use-package web-mode
;;   :mode  (("\\.html?\\'"    . web-mode)
;;           ("\\.erb\\'"      . web-mode)
;;           ("\\.ejs\\'"      . web-mode)
;;           ("\\.eex\\'"      . web-mode)
;;           ("\\.handlebars\\'" . web-mode)
;;           ("\\.hbs\\'"        . web-mode)
;;           ("\\.eco\\'"        . web-mode)
;;           ("\\.ect\\'"      . web-mode)
;;           ("\\.as[cp]x\\'"  . web-mode)
;;           ("\\.mustache\\'" . web-mode)
;;           ("\\.dhtml\\'"    . web-mode))
;;   :config
;;   (progn

;;     (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;       (if (equal web-mode-content-type "jsx")
;;           (let ((web-mode-enable-part-face nil))
;;             ad-do-it)
;;         ad-do-it))

;;     (defun grass/web-mode-hook ()
;;       "Hooks for Web mode."
;;       (setq web-mode-markup-indent-offset 2)
;;       (setq web-mode-css-indent-offset 2)
;;       (setq web-mode-code-indent-offset 2)
;;       (setq web-mode-enable-comment-keywords t)
;;       ;; Use server style comments
;;       (setq web-mode-comment-style 2)
;;       (define-key web-mode-map (kbd "C-, z") 'web-mode-fold-or-unfold)
;;       (setq web-mode-enable-current-element-highlight t))
;;     (add-hook 'web-mode-hook  'grass/web-mode-hook)))

;; ;; Setup for jsx
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

;; (use-package jade-mode
;;   :mode "\\.jade$"
;;   :config
;;   (require 'sws-mode)
;;   (require 'stylus-mode)
;;   (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode)))

;; (use-package scss-mode
;;   :mode "\\.scss$"
;;   :config
;;   (use-package rainbow-mode)
;;   (add-hook 'scss-mode-hook
;;             (lambda ()
;;               ;; Treat dollar and hyphen as a word character
;;               (modify-syntax-entry ?$ "w")
;;               (modify-syntax-entry ?- "w")
;;               (nlinum-mode 1)
;;               (rainbow-mode +1))))

;; (use-package css-mode
;;   :mode "\\.css$"
;;   :config
;;   (use-package rainbow-mode)
;;   (add-hook 'css-mode-hook
;;             (lambda ()
;;               (nlinum-mode 1)
;;               (rainbow-mode +1))))

;; ;;;;;;;;;;;;;;
;; ;; Markdown ;;
;; ;;;;;;;;;;;;;;

;; (use-package markdown-mode
;;   :mode (("\\.markdown\\'"    . markdown-mode)
;;          ("\\.md\\'"    . markdown-mode))
;;   :config
;;   (setq-default markdown-command "pandoc -S -s --self-contained -f markdown -t html5 ")

;;   (defun grass/markdown-enter-key-dwim ()
;;     "If in a list enter a new list item, otherwise insert enter key as normal."
;;     (interactive)
;;     (let ((bounds (markdown-cur-list-item-bounds)))
;;       (if bounds
;;           ;; In a list
;;           (call-interactively #'markdown-insert-list-item)
;;         ;; Not in a list
;;         (markdown-enter-key))))

;;   (define-key markdown-mode-map (kbd "RET") 'grass/markdown-enter-key-dwim)

;;   ;; Keep word movement instead of promotion mappings
;;   (define-key markdown-mode-map (kbd "<M-right>") nil)
;;   (define-key markdown-mode-map (kbd "<M-left>") nil)

;;   (setq markdown-imenu-generic-expression
;;         '(("title"  "^\\(.*\\)[\n]=+$" 1)
;;           ("h2-"    "^\\(.*\\)[\n]-+$" 1)
;;           ("h1"   "^# \\(.*\\)$" 1)
;;           ("h2"   "^## \\(.*\\)$" 1)
;;           ("h3"   "^### \\(.*\\)$" 1)
;;           ("h4"   "^#### \\(.*\\)$" 1)
;;           ("h5"   "^##### \\(.*\\)$" 1)
;;           ("h6"   "^###### \\(.*\\)$" 1)
;;           ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

;;   (use-package pandoc-mode
;;     :diminish pandoc-mode)

;;   (add-hook 'markdown-mode-hook
;;       (lambda ()
;;         ;; Remove for now as they interfere with indentation
;;         ;; (define-key yas-minor-mode-map [(tab)] nil)
;;         ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;;         (setq imenu-generic-expression markdown-imenu-generic-expression)))

;;   (add-hook 'markdown-mode-hook 'pandoc-mode)

;;   ;; Preview markdown file in Marked.app
;;   (defun grass/markdown-open-marked ()
;;     "run Marked.app on the current file and revert the buffer"
;;     (interactive)
;;     (shell-command
;;      (format "open -a 'Marked 2' %s"
;;              (shell-quote-argument (buffer-file-name)))))
;;   (define-key markdown-mode-map (kbd "C-, u p") 'grass/markdown-open-marked))


;; ;;;;;;;;;;;;;
;; ;; Haskell ;;
;; ;;;;;;;;;;;;;

;; ;; Install some useful packages so this all works
;; ;; cabal update && cabal install happy hasktags stylish-haskell present ghc-mod hlint

;; (use-package haskell-mode
;;   :defer t
;;   :config
;;   (progn

;;     ;; Use hi2 for indentation
;;     (use-package hi2
;;       :config
;;       (setq hi2-show-indentations nil)
;;       (add-hook 'haskell-mode-hook 'turn-on-hi2))

;;     (use-package ghc
;;       :config
;;       (autoload 'ghc-init "ghc" nil t)
;;       (autoload 'ghc-debug "ghc" nil t)
;;       (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

;;     (use-package company-ghc
;;       :disabled t
;;       :config
;;       (add-to-list 'company-backends 'company-ghc) (custom-set-variables '(company-ghc-show-info t)))

;;     ; Make Emacs look in Cabal directory for binaries
;;     (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;       (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;       (add-to-list 'exec-path my-cabal-path))

;;     ; Add F8 key combination for going to imports block
;;     (eval-after-load 'haskell-mode
;;       '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;;     (setq haskell-indentation-disable-show-indentations t)

;;     ; Set interpreter to be "stack ghci"
;;     (setq haskell-process-type 'ghci)
;;     (setq haskell-process-path-ghci "stack")
;;     (setq haskell-process-args-ghci '("ghci"))
;;     (setq tab-always-indent t)

;;     ; Set interpreter to be "cabal repl"
;;     ;(setq haskell-process-type 'cabal-repl)

;;     ; Add key combinations for interactive haskell-mode
;;     (eval-after-load 'haskell-mode '(progn
;;                                       (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;                                       (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;                                       (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;                                       (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;                                       (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;                                       (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;                                       (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
;;                                       (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
;;     (eval-after-load 'haskell-cabal '(progn
;;                                        (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;                                        (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;                                        (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;                                        (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;                                        (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

;;                                        ; Set interpreter to be "stack ghci"
;;                                        (setq haskell-interactive-popup-errors nil)
;;                                        (setq haskell-process-type 'ghci)
;;                                        (setq haskell-process-path-ghci "stack")
;;                                        (setq haskell-process-args-ghci '("ghci"))))

;;     (eval-after-load 'haskell-mode
;;       '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
;;     (eval-after-load 'haskell-cabal
;;       '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))))


;; ;;;;;;;;;;
;; ;; Lisp ;;
;; ;;;;;;;;;;

;; (use-package clojure-mode
;;   :defer t
;;   :config

;;   (use-package flycheck-clojure
;;     :init
;;     (eval-after-load 'flycheck '(flycheck-clojure-setup)))

;;   (add-hook 'clojure-mode-hook #'flycheck-mode)

;;   (use-package clojure-snippets)

;;   (use-package cider
;;     :pin melpa-stable
;;     :init
;;     ;; REPL history file
;;     (setq cider-repl-history-file "~/.emacs.d/cider-history")

;;     ;; nice pretty printing
;;     (setq cider-repl-use-pretty-printing t)

;;     ;; nicer font lock in REPL
;;     (setq cider-repl-use-clojure-font-lock t)

;;     ;; result prefix for the REPL
;;     (setq cider-repl-result-prefix ";; => ")

;;     ;; never ending REPL history
;;     (setq cider-repl-wrap-history t)

;;     ;; looong history
;;     (setq cider-repl-history-size 3000)

;;     ;; error buffer not popping up
;;     (setq cider-show-error-buffer nil)

;;     ;; eldoc for clojure
;;     (add-hook 'cider-mode-hook #'eldoc-mode)

;;     ;; company mode for completion
;;     (add-hook 'cider-repl-mode-hook #'company-mode)
;;     (add-hook 'cider-mode-hook #'company-mode))

;;   (use-package clj-refactor
;;     :pin melpa-stable
;;     :init
;;     (add-hook 'clojure-mode-hook
;;               (lambda ()
;;                 (clj-refactor-mode 1)

;;                 ;; no auto sort
;;                 (setq cljr-auto-sort-ns nil)

;;                 ;; do not prefer prefixes when using clean-ns
;;                 (setq cljr-favor-prefix-notation nil)
;;                 ;; insert keybinding setup here
;;                 (cljr-add-keybindings-with-prefix "C-c RET")))))

;; (add-hook 'emacs-lisp-mode-hook
;;   (lambda ()
;;     (define-key global-map (kbd "C-c C-e") 'eval-print-last-sexp)))


;; ;;;;;;;;;;;;;;;;;;;;;
;; ;; Other Languages ;;
;; ;;;;;;;;;;;;;;;;;;;;;

;; (use-package elixir-mode
;;   :mode (("\\.exs?\\'"   . elixir-mode)
;;          ("\\.elixer\\'" . elixir-mode))
;;   :defer t
;;   :config
;;   (use-package alchemist))

;; (use-package puppet-mode
;;   :defer t)

;; (use-package powershell
;;   :defer t
;;   :mode  (("\\.ps1$" . powershell-mode)
;;           ("\\.psm$" . powershell-mode)))

;; (use-package rust-mode
;;   :defer t)

;; (use-package python
;;   :defer t)

;; (use-package yaml-mode
;;   :defer t)

;; (use-package haml-mode
;;   :defer t
;;   :mode "\\.haml$"
;;   :config
;;   (add-hook 'haml-mode-hook
;;     (lambda ()
;;       (set (make-local-variable 'tab-width) 2))))

;; ;;;;;;;;;;;;;;
;; ;; Spelling ;;
;; ;;;;;;;;;;;;;;

;; (use-package flyspell
;;   :defer t
;;   :commands flyspell-mode
;;   :diminish (flyspell-mode . " spl")
;;   :config
;;   (setq-default ispell-program-name "aspell")
;;   ; Silently save my personal dictionary when new items are added
;;   (setq ispell-silently-savep t)
;;   (ispell-change-dictionary "en_GB" t)

;;   (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
;;   (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

;;   ;; Spell checking in comments
;;   ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;   (which-key-declare-prefixes "C-, S" "spelling")
;;   (add-hook 'flyspell-mode-hook
;;             (lambda ()
;;               (define-key flyspell-mode-map [(control ?\,)] nil)
;;               (global-set-key (kbd "C-, S n") 'flyspell-goto-next-error)
;;               (global-set-key (kbd "C-, S w") 'ispell-word))))

(use-package spaceline
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'bar)
    (setq spaceline-minor-modes-separator "⋅")
    (setq spaceline-higlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-emacs-theme)
    (spaceline-info-mode)))

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(global-set-key [s-up] 'dired-jump)

(general-define-key :states '(normal visual) :prefix grass/leader1
		    "TAB" 'grass/switch-to-previous-buffer)
                    ; TODO Make these work
		    ;"v o" 'turn-on-visual-line-mode
		    ;"v f" 'turn-off-visual-line-mode)

