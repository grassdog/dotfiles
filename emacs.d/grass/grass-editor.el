
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

;; Saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" grass/savefile-dir))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; Don't make tab indent a line
(setq tab-always-indent nil)

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
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

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

;; Base 10 for inserting quoted chars please
(setq read-quoted-char-radix 10)

;; Dashes
(defun grass/insert-en-dash
  "Insert an en dash"
  (interactive)
  (insert "–"))

(defun grass/insert-em-dash
  "Insert an en dash"
  (interactive)
  (insert "—"))

;; Dashes
(global-set-key (kbd "C-, -") 'grass/insert-en-dash)
(global-set-key (kbd "C-, =") 'grass/insert-em-dash)

;; Auto save on focus lost
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


;; Reuse the same buffer for dired windows
(use-package dired-single
  :init
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's loaded."
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
    (define-key dired-mode-map "U"
      (function
       (lambda nil (interactive) (dired-single-buffer ".."))))
    (define-key dired-mode-map "^"
      (function
       (lambda nil (interactive) (dired-single-buffer "..")))))

  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))
  (put 'dired-find-alternate-file 'disabled nil))

;; Up in dired
(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode t)))

;; Make files with the same name have unique buffer names
(setq uniquify-buffer-name-style 'forward)

;; Some key bindings
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(use-package move-text
  :bind (("<C-S-up>" . move-text-up)
         ("<C-S-down>" . move-text-down)))

(global-set-key (kbd "C-, u") 'browse-url)
(global-set-key (kbd "C-, i") 'grass/indent-region-or-buffer)

;; Quick switch buffers
(global-set-key (kbd "C-, C-,") 'grass/switch-to-previous-buffer)

(global-set-key (kbd "C-, s r") 'query-replace-regexp)
(global-set-key (kbd "C-, s f") 'isearch-forward-regexp)
(global-set-key (kbd "C-, s b") 'isearch-reverse-regexp)

;; Enable some stuff
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;
;; Make windows sticky http://stackoverflow.com/a/5182111
;;
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-set-key (kbd "C-, q") 'toggle-window-dedicated)

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  ;; Persistent undo sometimes borks. Disable for now
  ;; (setq undo-tree-auto-save-history t)
  ;; (setq undo-tree-history-directory-alist `((".*" . ,grass/undo-dir)))
  ;; (defadvice undo-tree-make-history-save-file-name
  ;;   (after undo-tree activate)
  ;;   (setq ad-return-value (concat ad-return-value ".gz")))
  (global-undo-tree-mode))


;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("C-, e" . yas/expand)
  :config
  (setq yas-verbosity 1)
  (yas-global-mode 1))

(provide 'grass-editor)
