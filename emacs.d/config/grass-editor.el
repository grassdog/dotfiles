;; Tabbage
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; Follow symlinks by default
(setq vc-follow-symlinks t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; 80 char wide paragraphs please
(setq-default fill-column 80)

;; Autofill where possible but only in comments when coding
;; http://stackoverflow.com/questions/4477357/how-to-turn-on-emacs-auto-fill-mode-only-for-code-comments
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

;; Make scratch text mode by default
;(setq initial-major-mode 'text-mode)

;; Text mode by default for new buffers
(setq default-major-mode 'text-mode)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; TODO: Does this expand setup work?

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(yas/hippie-try-expand
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

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

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

(defcustom prelude-auto-save t
  "Non-nil values enable Prelude's auto save."
  :type 'boolean
  :group 'prelude)

(defcustom prelude-whitespace t
  "Non-nil values enable Prelude's whitespace visualization."
  :type 'boolean
  :group 'prelude)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun prelude-auto-save-command ()
  (when (and prelude-auto-save
             buffer-file-name
             (buffer-modified-p (current-buffer)))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (prelude-auto-save-command))
(defadvice other-window (before other-window-now activate)
  (prelude-auto-save-command))
(defadvice windmove-up (before other-window-now activate)
  (prelude-auto-save-command))
(defadvice windmove-down (before other-window-now activate)
  (prelude-auto-save-command))
(defadvice windmove-left (before other-window-now activate)
  (prelude-auto-save-command))
(defadvice windmove-right (before other-window-now activate)
  (prelude-auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'prelude-auto-save-command)

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" grass-history-dir)
      ido-default-file-method 'selected-window)

;; Insert ~ anywhere to go straight home
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

;; auto-completion in minibuffer
(icomplete-mode +1)

;; Sections in files
(set-default 'imenu-auto-rescan t)

(defun prelude-enable-whitespace ()
  (when prelude-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'whitespace-cleanup nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'prelude-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'expand-region)

;; bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" grass-history-dir)
      bookmark-save-flag 1)

;; load yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs grass-snippets-dir)
(yas-global-mode 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" grass-history-dir))
(projectile-global-mode t)

(require 'helm-misc)
(require 'helm-projectile)

(defun helm-prelude ()
  "Preconfigured `helm'."
  (interactive)
  (condition-case nil
      (if (projectile-project-root)
          ;; add project files and buffers when in project
          (helm-other-buffer '(helm-c-source-projectile-files-list
                               helm-c-source-projectile-buffers-list
                               helm-c-source-buffers-list
                               helm-c-source-recentf
                               helm-c-source-buffer-not-found)
                             "*helm prelude*")
        ;; otherwise fallback to helm-mini
        (helm-mini))
    ;; fall back to helm mini if an error occurs (usually in projectile-project-root)
    (error (helm-mini))))

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; ediff - don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(clojure-mode scala-mode python-mode LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped). Only
modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; whitespace-mode config
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs trailing))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" grass-history-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" grass-history-dir))

;; Wrap Region
(require 'wrap-region)

;; Active in all modes
;; TODO: Might be worth disabling this if it gets in the way too much
(wrap-region-global-mode t)

;; Wrap region screws with the mini buffer so disable it there
(add-hook 'minibuffer-setup-hook
          (lambda () (turn-off-wrap-region-mode)))

;; Don't screw up key bindings in magit-mode
(add-to-list 'wrap-region-except-modes 'magit-mode)

;; Custom wrappers

(wrap-region-add-wrapper "{ value: " " }" "v" 'js2-mode)
(wrap-region-add-wrapper "$(" ")" "$" 'js2-mode)

(wrap-region-add-wrapper "<p>" "</p>" "p" 'html-mode)
(wrap-region-add-wrapper "<li>" "</li>" "l" 'html-mode)
(wrap-region-add-wrapper "<strong>" "</strong>" "s" 'html-mode)
(wrap-region-add-wrapper "<a href=\"\">" "</a>" "a" 'html-mode)
(wrap-region-add-wrapper "<h1>" "</h1>" "h" 'html-mode)
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode ruby-mode))

;; A smarter TAB [EmacsWiki](http://emacswiki.org/emacs/TabCompletion)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
    (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

(global-set-key (kbd "TAB") 'smart-tab)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Common files

(defun open-cheats ()
  "Open Emacs cheats file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs.md"))


(provide 'grass-editor)
