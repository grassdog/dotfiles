
;; Follow symlinks by default
(setq vc-follow-symlinks t)

;; Selections and other actions
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
(setq save-place-file (expand-file-name "saveplace" grass/savefile-dir))
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
      savehist-file (expand-file-name "savehist" grass/savefile-dir))
(savehist-mode t)

;; Save recent files
(setq recentf-save-file (expand-file-name "recentf" grass/savefile-dir)
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; Subtle highlighting of matching parens (global-mode)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; UI highlight search and other actions
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--lang=en_AU --sug-mode=ultra"))

;; 80 char wide paragraphs please
(setq-default fill-column 80)

;; Autofill where possible but only in comments when coding
;; http://stackoverflow.com/questions/4477357/how-to-turn-on-emacs-auto-fill-mode-only-for-code-comments
;; (auto-fill-mode 1)
;; (setq comment-auto-fill-only-comments t)


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

(provide 'grass-editor)
