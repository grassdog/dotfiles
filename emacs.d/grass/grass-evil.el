
;; Trojan horse maneuver

(use-package evil
  :config

  ;; Evil plugins
  (use-package evil-commentary
    :diminish evil-commentary-mode
    :init
    (evil-commentary-mode))

  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1))

  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))

  (use-package evil-visualstar
    :init
    (global-evil-visualstar-mode))

  (use-package evil-search-highlight-persist
    :init
    (global-evil-search-highlight-persist t)
    (evil-search-highlight-persist -1)

  ;; Evil config

  ; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (setq evil-shift-width 2)
  (require 'evil-little-word)

  (define-key evil-normal-state-map (kbd "SPC") 'evil-search-highlight-persist-remove-all))

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
  (define-key evil-normal-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-visual-state-map "\C-w" 'evil-delete)
  (define-key evil-normal-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-visual-state-map "\C-y" 'yank)
  (define-key evil-normal-state-map "\C-k" 'kill-line)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-visual-state-map "\C-k" 'kill-line)
  (define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
  (define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
  ;;(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)

  ;; Set our default modes
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
                                (help-mode . emacs)
                                (helm-grep-mode . emacs)
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
                                (bs-mode . emacs)
                                (dired-mode . emacs)
                                (wdired-mode . normal))
        do (evil-set-initial-state mode state)))

(provide 'grass-evil)
