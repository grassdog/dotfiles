;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

;; Editing
(setq require-final-newline t)
(setq sentence-end-double-space nil)

;; Follow symlinks by default
(setq vc-follow-symlinks t)

;; Don't combine tag tables thanks
(setq tags-add-tables nil)

;; World times
(setq display-time-world-list '(("Australia/Brisbane" "Brisbane")
                                ("Australia/Melbourne" "Melbourne")
                                ("Europe/London" "London")
                                ("America/New_York" "New York")
                                ("America/Los_Angeles" "San Francisco")))

;; Some terminal mapping hackery to accept C-, key sequence mapping from iTerm
(defadvice terminal-init-xterm
  (after map-C-comma-escape-sequence activate)
  (define-key input-decode-map "\e[1;," (kbd "C-,")))


(setq linum-format "%4d ")

;; Soft line wrapping for text modes
(spacemacs|diminish visual-line-mode " w" " w")
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;;;;;;;;;;;;
;; Movement ;;
;;;;;;;;;;;;;;

;; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

(setq-default tab-width 2)
(setq evil-shift-width 2)
(setq lisp-indent-offset 2)
(setq-default js2-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default js-indent-level 2)
(setq css-indent-offset 2)
(setq coffee-tab-width 2)
(setq-default py-indent-offset 2)
(setq-default nxml-child-indent 2)

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

(spacemacs|diminish elm-indent-mode " i" " i")
;; This is getting in the way more than not at the moment
(add-hook 'elm-mode-hook
  (lambda ()
    (setq evil-shift-width 4)
    (setq tab-width 4)
    (setq elm-indent-offset 4)
    ;; (electric-indent-mode -1)
    ;; (clean-aindent-mode t)
    ;; (setq clean-aindent-is-simple-indent t)
    ;; (elm-indent-mode -1)
    ))

(spacemacs|diminish rubocop-mode " R" " R")
;; Indentation
(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil)
    (setq ruby-use-smie nil)
    (setq evil-shift-width 2)))


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

;; Line numbers for coding please
(add-hook 'prog-mode-hook
  (lambda ()
    (linum-mode 1)
    ;; Treat underscore as a word character
    ;; evil-little-word allows for finer grain editing
    (modify-syntax-entry ?_ "w")))

;;;;;;;;;;;;;;;;
;; Javascript ;;
;;;;;;;;;;;;;;;;

(setq js2-bounce-indent-p t
  js2-strict-trailing-comma-warning nil
  js2-strict-missing-semi-warning nil)

(setq-default js2-global-externs
  '("module" "require" "__dirname" "process" "console" "define"
    "JSON" "$" "_" "Backbone" "buster" "sinon" "moment" "_gaq"))

(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))


;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;

(setq-default ispell-program-name "aspell")
;; Silently save my personal dictionary when new items are added
(setq ispell-silently-savep t)
(ispell-change-dictionary "en_GB" t)

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(with-eval-after-load 'org
  ;; Start up fully open
  (setq org-startup-folded nil)

  ;; Make windmove work in org-mode
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link nil)
  (setq org-agenda-files '("~/Dropbox/Notes"))

  (setq org-log-done nil)

  ;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil)
  ;; Allow bind in files to enable export overrides
  (setq org-export-allow-bind-keywords t)
  (defun grass/html-filter-remove-src-blocks (text backend info)
    "Remove source blocks from html export."
    (when (org-export-derived-backend-p backend 'html) ""))

  (spacemacs|add-toggle org-descriptive-links
    :status nil
    :on (org-toggle-link-display)
    :documentation "Toggle display of link details in org mode."
    :evil-leader "tO")

  ;; Highlight source blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)

  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (add-hook 'org-mode-hook
    (lambda ()
      ;; No auto indent please
      ;;(setq evil-auto-indent nil)
      (setq org-export-html-postamble nil)
      ;; Let me keep my prefix key binding
      (define-key org-mode-map (kbd "C-,") nil)
      (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(defun grass/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."

  (setq dired-use-ls-dired nil)
  (setq dired-omit-files
    (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
          (seq "~" eol)                 ;; backup-files
          (seq bol "CVS" eol)           ;; CVS dirs
          (seq ".pyc" eol)
          (seq bol ".DS_Store" eol)
          (seq bol ".tern-port" eol))))

  (evil-leader/set-key-for-mode 'dired-mode
    "g" 'revert-buffer)

  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "<s-up>")
    (function
      (lambda nil (interactive) (dired-single-buffer "..")))))

(if (boundp 'dired-mode-map)
  (grass/dired-init)
  (add-hook 'dired-load-hook 'grass/dired-init))

(add-hook 'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode t)
    (dired-filter-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile and ignored files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'projectile-load-hook
  (lambda ()
    (setq projectile-tags-command "getags")
    ;; (setq projectile-enable-caching t)
    (setq projectile-hg-command "( hg locate -0 -I . ; hg st -u -n -0 )")
    (add-to-list 'projectile-globally-ignored-directories "gems")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "bower_components")
    (add-to-list 'projectile-globally-ignored-directories "dist")
    (add-to-list 'projectile-globally-ignored-directories "/emacs.d/elpa/")
    (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
    (add-to-list 'projectile-globally-ignored-files ".tern-port")
    (add-to-list 'projectile-globally-ignored-files ".keep")
    (add-to-list 'projectile-globally-ignored-files "TAGS")))

(add-hook 'recentf-load-hook
  (lambda ()
    (add-to-list 'recentf-exclude "\\ido.hist\\'")
    (add-to-list 'recentf-exclude "/TAGS")
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sane line killing in Emacs mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;
;; Smartparens ;;
;;;;;;;;;;;;;;;;;

(with-eval-after-load 'smartparens
  ;; No auto pairing of quotes thanks
  (sp-pair "'" nil :actions '(:rem insert))
  (sp-pair "\"" nil :actions '(:rem insert))

  (defun grass/elixir-do-end-close-action (id action context)
    (when (eq action 'insert)
      (newline-and-indent)
      (previous-line)
      (indent-according-to-mode)))

  (defun grass/elixir-fn-end-close-action (id action context)
    (when (eq action 'insert)
      (newline-and-indent)
      (previous-line)
      (indent-according-to-mode)
      (end-of-line)
      (insert " ")))

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(:add grass/elixir-do-end-close-action)
                   :actions '(insert))

    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   ;; :post-handlers '(:add grass/elixir-fn-end-close-action)
                   :actions '(insert))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Better pop to mark ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; When popping the mark, continue popping until the cursor actually moves
;; http://endlessparentheses.com/faster-pop-to-mark-command.html
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; Multiple pop to marks via C-u C-SPC C-SPC
(setq set-mark-command-repeat-pop t)

;;;;;;;;;;;;;;
;; Web Mode ;;
;;;;;;;;;;;;;;

(add-hook 'web-mode-hook
  (lambda ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq evil-shift-width 2)
    (setq web-mode-enable-comment-keywords t)
    ;; Use server style comments
    (setq web-mode-comment-style 2)))
