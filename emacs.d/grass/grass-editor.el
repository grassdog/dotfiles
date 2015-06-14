
;; Tabs
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 2)            ;; but maintain correct appearance

;; Follow symlinks by default
(setq vc-follow-symlinks t)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Smart pairing for all
(electric-pair-mode t)

;; Saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" grass-savefile-dir))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; Savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" grass-savefile-dir))
(savehist-mode t)

;; Save recent files
(setq recentf-save-file (expand-file-name "recentf" grass-savefile-dir)
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; Subtle highlighting of matching parens (global-mode)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; Highlight the current line
(global-hl-line-mode +1)

;; UI highlight search and other actions
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Better suggestions
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" grass-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      org-completion-use-ido t)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;; smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" grass-savefile-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)




;; Trojan horse maneuver

;; Leaders
(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode)

(require 'evil)
(evil-mode t)

;; Interactive list refinement
(require 'helm-config)
(require 'helm-projectile)

(setq helm-buffers-fuzzy-matching t)
;(setq helm-split-window-in-side-p           t
;      helm-buffers-fuzzy-matching           t
;      helm-move-to-line-cycle-in-source     t
;      helm-ff-search-library-in-sexp        t
;      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-c h") 'helm-command-prefix)


;; Project file management
(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode t)

(setq projectile-completion-system 'helm)
(helm-projectile-on)


;; Common files

(defun grass-open-cheats ()
  "Open Emacs cheats file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs.md"))


(provide 'grass-editor)

