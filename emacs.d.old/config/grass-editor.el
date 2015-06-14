;; Tabbage
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 2)            ;; but maintain correct appearance

;; Follow symlinks by default
(setq vc-follow-symlinks t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; 80 char wide paragraphs please
(setq-default fill-column 80)

;; Trojan horse maneuver
(require 'evil)
(evil-mode t)

;; Autofill where possible but only in comments when coding
;; http://stackoverflow.com/questions/4477357/how-to-turn-on-emacs-auto-fill-mode-only-for-code-comments
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

;; Text mode by default for new buffers
(setq default-major-mode 'text-mode)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart pairing for all
(electric-pair-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" grass-history-dir))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" grass-history-dir))
(savehist-mode t)

;; save recent files
(setq recentf-save-file (expand-file-name "recentf" grass-history-dir)
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" grass-history-dir)
      ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)

;; bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" grass-history-dir)
      bookmark-save-flag 1)

;; whitespace-mode config
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs trailing))

;; Snippets
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs grass-snippets-dir)
(yas-global-mode 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode t)

(require 'helm-misc)
(require 'helm-projectile)

;; clean up obsolete buffers automatically
(require 'midnight)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" grass-history-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" grass-history-dir))

;; Common files

(defun open-cheats ()
  "Open Emacs cheats file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs.md"))

(provide 'grass-editor)
