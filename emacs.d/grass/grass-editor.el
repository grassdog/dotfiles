
;; Follow symlinks by default
(setq vc-follow-symlinks t)

;; Don't make tab indent a line
(setq tab-always-indent nil)

;; Don't combine tag tables thanks
(setq tags-add-tables nil)

;; Wrap lines for text modes
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Line numbers for coding please
(setq on-console (null window-system))
(setq linum-format (if on-console "%4d " "%4d"))

;; Show current function in modeline
(which-function-mode)

(set-default 'imenu-auto-rescan t)

(global-set-key (kbd "C-, i") 'imenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selections and other actions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Delete selected regions
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and editing history ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(use-package saveplace
  :config
  ;; Saveplace remembers your location in a file when saving files
  (setq save-place-file (expand-file-name "saveplace" grass/savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t)
  ;; Savehist keeps track of some history
  (setq savehist-additional-variables
        ;; search entries
        '(search ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" grass/savefile-dir))
  :init
  (savehist-mode t))

(use-package recentf
  :defer t
  :init
  ;; lazy load recentf
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                         (recentf-mode)
                                         (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude "\\ido.hist\\'")
  (add-to-list 'recentf-exclude "/TAGS")
  (add-to-list 'recentf-exclude "/.autosaves/")
  (add-to-list 'recentf-exclude "emacs.d/elpa/")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (setq recentf-save-file (expand-file-name "recentf" grass/savefile-dir))
  (setq recentf-max-saved-items 100))


;;;;;;;;;;;;;;;;;;;;;;;
;; Sane line killing ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; If no region kill or copy current line
;; http://emacs.stackexchange.com/questions/2347/kill-or-copy-current-line-with-minimal-keystrokes
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))


;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;


;; Subtle highlighting of matching parens (global-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (show-paren-mode +1)
                            (setq show-paren-style 'parenthesis)))

;; UI highlight search and other actions
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (require 'highlight)
  (volatile-highlights-mode t))

;; Use shift + arrow keys to switch between visible buffers
(use-package windmove
  :init
  (windmove-default-keybindings))

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . " spl")
  :init
  (setq-default ispell-program-name "aspell")
                                        ; Silently save my personal dictionary when new items are added
  (setq ispell-silently-savep t)
  (ispell-change-dictionary "en_GB" t)

  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
  (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

  ;; Spell checking in comments
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (add-hook 'flyspell-mode-hook
            (lambda ()
              (define-key flyspell-mode-map [(control ?\,)] nil)
              (global-set-key (kbd "C-, S n") 'flyspell-goto-next-error)
              (global-set-key (kbd "C-, S w") 'ispell-word))))

;; World times
(setq display-time-world-list '(("Australia/Brisbane" "Brisbane")
                                ("Australia/Melbourne" "Melbourne")
                                ("Europe/London" "London")
                                ("America/New_York" "New York")
                                ("America/Los_Angeles" "San Francisco")))

;; Utilities
(global-set-key (kbd "C-, u t") 'display-time-world)
(global-set-key (kbd "C-, u c") 'quick-calc)
(global-set-key (kbd "C-, u u") 'browse-url)
(global-set-key (kbd "C-, u r") 'grass/rename-file-and-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments and filling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(global-set-key (kbd "C-, u b") 'grass/comment-box)

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

;;;;;;;;;;;;;;;;;;;;;;
;; Symbol insertion ;;
;;;;;;;;;;;;;;;;;;;;;;


;; Base 10 for inserting quoted chars please
(setq read-quoted-char-radix 10)

(use-package smart-quotes
  :ensure nil
  :commands smart-quotes-mode)

;; Dashes
(defun grass/insert-en-dash ()
  "Insert an en dash"
  (interactive)
  (insert "–"))

(defun grass/insert-em-dash ()
  "Insert an en dash"
  (interactive)
  (insert "—"))

;; Dashes
(global-set-key (kbd "C-, -") 'grass/insert-en-dash)
(global-set-key (kbd "C-, =") 'grass/insert-em-dash)


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


;; abbrev mode for common typos
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(diminish 'abbrev-mode)
(setq-default abbrev-mode t)


;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;


;; Reuse the same buffer for dired windows
(use-package dired-single
  :config
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's loaded."
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
    (define-key dired-mode-map "U"
      (function
       (lambda nil (interactive) (dired-single-buffer ".."))))
    (define-key dired-mode-map "^"
      (function
       (lambda nil (interactive) (dired-single-buffer ".."))))
    (setq dired-use-ls-dired nil))

  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))
  (put 'dired-find-alternate-file 'disabled nil))

;; Up in dired
(use-package dired+
  :bind ("C-x C-j" . dired-jump))

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

(global-set-key (kbd "C-, f") 'grass/indent-region-or-buffer)

;; Quick switch buffers
(global-set-key (kbd "C-, C-,") 'grass/switch-to-previous-buffer)

;; Easier key binding for shell replace command
(defun grass/shell-command-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'shell-command-on-region))

(global-set-key (kbd "C-, !") 'grass/shell-command-with-prefix-arg)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and Replace ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defun grass/replace-string (from-string to-string &optional delimited start end)
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

(defun grass/replace-regexp (regexp to-string &optional delimited start end)
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

(defun grass/query-replace-regexp (regexp to-string &optional delimited start end)
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

(defun grass/query-replace-string (from-string to-string &optional delimited start end)
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

(global-set-key (kbd "C-, s r") 'grass/replace-string)
(global-set-key (kbd "C-, s R") 'grass/replace-regexp)
(global-set-key (kbd "C-, s q") 'grass/query-replace-string)
(global-set-key (kbd "C-, s Q") 'grass/query-replace-regexp)
(global-set-key (kbd "C-, s f") 'isearch-forward-regexp)
(global-set-key (kbd "C-, s b") 'isearch-reverse-regexp)

;; Enable some stuff
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert current word into minibuffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make windows sticky http://stackoverflow.com/a/5182111 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;
;; Region selection ;;
;;;;;;;;;;;;;;;;;;;;;;


(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)
         ("<s-down>" . er/contract-region)
         ("<s-up>" . er/expand-region)))

(use-package ag
  :bind (("C-, s a" . ag-project))
  :commands ag-project)

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

;; Keep system clipboard separate from kill ring
(use-package simpleclip
  :init
  (simpleclip-mode 1))

;;;;;;;;;;;;
;; Coding ;;
;;;;;;;;;;;;

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\|DEBUG\\|GRASS\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


;;;;;;;;;;;;;;;;;;
;; Autocomplete ;;
;;;;;;;;;;;;;;;;;;


(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-global-modes
        '(not markdown-mode org-mode erc-mode))

  (define-key company-active-map [escape] 'company-abort)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  :diminish yas-minor-mode
  :init

  (defun grass/do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas-expand)))

  (defun grass/check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun grass/tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      (indent-for-tab-command)
      (if (or (not yas-minor-mode)
              (null (grass/do-yas-expand)))
          (if (grass/check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (indent-for-tab-command)))))))))

  (defun grass/tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (grass/do-yas-expand)))
        (if company-candidates
            (company-complete-selection)
          (if (grass/check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (yas-next-field))))
            (yas-next-field)))))

  (defun grass/expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (grass/do-yas-expand))
            (company-abort))
        (company-complete-selection)))

  (defun grass/abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

  (setq yas-verbosity 1)
  (yas-global-mode 1)

  (global-set-key [tab] 'grass/tab-indent-or-complete)
  (global-set-key (kbd "TAB") 'grass/tab-indent-or-complete)
  (global-set-key [(control return)] 'company-complete-common)

  (define-key company-active-map [tab] 'grass/expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'grass/expand-snippet-or-complete-selection)

  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; Don't enable smartparens when expanding
  (defvar smartparens-enabled-initially t
    "Stored whether smartparens is originally enabled or not.")

  (add-hook 'yas-before-expand-snippet-hook (lambda ()
                                              ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
                                              (setq smartparens-enabled-initially smartparens-mode)
                                              (smartparens-mode -1)))
  (add-hook 'yas-after-exit-snippet-hook (lambda ()
                                           (when smartparens-enabled-initially
                                             (smartparens-mode 1))))

  (define-key yas-keymap [tab] 'grass/tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'grass/tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'grass/abort-company-or-yas)
  (define-key yas-minor-mode-map (kbd "C-, e") 'yas-expand))

(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html))

(use-package flycheck
  :defer t
  :config

  (use-package flycheck-tip
    :bind ("C-, C-n" . flycheck-tip-cycle))

  (use-package flycheck-pos-tip
    :disabled
    :init
    (eval-after-load 'flycheck
      '(setq flycheck-display-errors-function
             #'flycheck-pos-tip-error-messages))))


(use-package magit
  :bind ("C-, g" . magit-status))

(use-package string-inflection
  :bind ("C-, C-i" . string-inflection-cycle))

(use-package hideshow
  :diminish hs-minor-mode)


;;;;;;;;;;;;;;
;; Wrapping ;;
;;;;;;;;;;;;;;


(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  ;; Wrap an entire symbol
  (setq sp-wrap-entire-symbol nil)

  ;; Don't autoescape when inside a quote
  ;; (setq sp-autoescape-string-quote nil)

  ;; No auto pairing of quotes thanks
  ;; (sp-local-pair 'enh-ruby-mode "'" nil :actions '(:rem insert))
  ;; (sp-local-pair 'enh-ruby-mode "\"" nil :actions '(:rem insert))

  (add-hook 'enh-ruby-mode-hook #'smartparens-mode)
  (add-hook 'js2-mode-hook #'smartparens-mode)
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode))

(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(bind-keys
 :map smartparens-mode-map
 ("C-, l r"  . sp-rewrap-sexp)
 ("C-, l d"  . sp-unwrap-sexp)

 ("C-, l ("  . wrap-with-parens)
 ("C-, l ["  . wrap-with-brackets)
 ("C-, l {"  . wrap-with-braces)
 ("C-, l '"  . wrap-with-single-quotes)
 ("C-, l \"" . wrap-with-double-quotes)
 ("C-, l _"  . wrap-with-underscores)
 ("C-, l `"  . wrap-with-back-quotes))


;;;;;;;;;;;;;
;; Alignment
;;;;;;;;;;;;;

;; modified function from http://emacswiki.org/emacs/AlignCommands
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

;; Bindings
(global-set-key (kbd "C-, a a") 'align)
(global-set-key (kbd "C-, a r") 'align-repeat)
(global-set-key (kbd "C-, a m") 'align-repeat-math-oper)
(global-set-key (kbd "C-, a .") 'align-repeat-decimal)
(global-set-key (kbd "C-, a ,") 'align-repeat-comma)
(global-set-key (kbd "C-, a ;") 'align-repeat-semicolon)
(global-set-key (kbd "C-, a :") 'align-repeat-colon)
(global-set-key (kbd "C-, a =") 'align-repeat-equal)
(global-set-key (kbd "C-, a >") 'align-repeat-hash)
(global-set-key (kbd "C-, a &") 'align-repeat-ampersand)
(global-set-key (kbd "C-, a |") 'align-repeat-bar)
(global-set-key (kbd "C-, a (") 'align-repeat-left-paren)
(global-set-key (kbd "C-, a )") 'align-repeat-right-paren)


;;;;;;;;;;;;;;;
;; Prog mode ;;
;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode)
            (hs-minor-mode t)
            ;; Treat underscore as a word character
            (modify-syntax-entry ?_ "w")))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package puppet-mode
  :defer t)

(use-package powershell
  :mode  (("\\.ps1$" . powershell-mode)
          ("\\.psm$" . powershell-mode)))

(use-package rust-mode
  :defer t)

(use-package python
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation styles et al ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Default indentation
(setq-default tab-width 4)

;; Javascript
(setq-default js2-basic-offset 2)

;; JSON
(setq-default js-indent-level 2)

;; Sass
(setq css-indent-offset 2)

;; Coffeescript
(setq coffee-tab-width 2)

;; Python
(setq-default py-indent-offset 2)

;; XML
(setq-default nxml-child-indent 2)

;; Ruby
(setq ruby-indent-level 4)

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

(setq sentence-end-double-space nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enforce proper whitespace ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'whitespace)
(diminish 'global-whitespace-mode)

(setq require-final-newline t)

;; Auto whitespace cleanup
(defun grass/disable-auto-whitespace-cleanup ()
  "Disables whitespace-cleanup on save."
  (interactive)
  (remove-hook 'before-save-hook 'whitespace-cleanup))

(defun grass/enable-auto-whitespace-cleanup ()
  "Enables whitespace-cleanup on save."
  (interactive)
  (add-hook 'before-save-hook 'whitespace-cleanup))

(grass/enable-auto-whitespace-cleanup)

(define-key global-map (kbd "C-, w") 'whitespace-cleanup)

;; Only show bad whitespace (Ignore empty lines at start and end of buffer)
(setq whitespace-style '(face tabs trailing space-before-tab indentation space-after-tab))
(global-whitespace-mode t)


(provide 'grass-editor)
