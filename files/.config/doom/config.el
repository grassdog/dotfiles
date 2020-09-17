;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ray Grasso"
      user-mail-address "ray.grasso@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Set font based upon screen height
(let* ((geometry (frame-monitor-geometry))
       (height (nth 3 geometry)))
  (if (> height 1000)
      (setq doom-font (font-spec :family "Operator Mono" :size 14 :weight 'light))
    (setq doom-font (font-spec :family "Operator Mono" :size 13 :weight 'light))))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Notes")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Cross lines thanks
(setq-default evil-cross-lines t)

;; No moving the cursor back when exiting insert mode
(setq evil-move-cursor-back nil)

;; Give me four lines from the bottom
(setq scroll-margin 4)
(setq scroll-conservatively 100000)

(setq-default tab-width 2)

;; Arrow key window movement
(after! evil!
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down))

(use-package! evil-better-visual-line
  :after evil
  :config
  (evil-better-visual-line-on)
  (define-key evil-operator-state-map (kbd "<down>") #'evil-better-visual-line-next-line)
  (define-key evil-normal-state-map (kbd "<down>") #'evil-better-visual-line-next-line)
  (define-key evil-visual-state-map (kbd "<down>") #'evil-better-visual-line-next-line)
  (define-key evil-operator-state-map (kbd "<up>") #'evil-better-visual-line-previous-line)
  (define-key evil-normal-state-map (kbd "<up>") #'evil-better-visual-line-previous-line)
  (define-key evil-visual-state-map (kbd "<up>") #'evil-better-visual-line-previous-line))


;; Just quit thanks
(setq confirm-kill-emacs nil)

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

;; Useful buffer
(defun grass/useful-buffer-p (&optional potential-buffer-name)
  "Return t if current buffer is a user buffer, else nil."
  (interactive)
  (let ((buffer-to-test (or potential-buffer-name (buffer-name))))
    (if (string-equal "*" (substring (s-trim-left buffer-to-test) 0 1))
        nil
      (if (string-match "dired" (symbol-name
                                 (with-current-buffer potential-buffer-name
                                   major-mode)))
          nil
        t))))

(defun grass/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (let* ((candidate-buffers (cl-remove-if-not
                             #'grass/useful-buffer-p
                             (mapcar (function buffer-name) (buffer-list))))
         (candidate-buffer (nth 1 candidate-buffers)))
    (if candidate-buffer
        (switch-to-buffer (nth 1 candidate-buffers)))))

;; Common files

(defun grass/open-work-log ()
  "Open Worklog file"
  (interactive)
  (find-file "~/Dropbox/Notes/Work/Envato/Work.org"))

(defun grass/find-notes ()
  "Find a note in Dropbox/Notes directory"
  (interactive)
  (counsel-file-jump "" (expand-file-name "~/Dropbox/Notes")))

;; Common searches

(defun grass/search-work-notes (&optional symbol)
  "Conduct a text search across my work notes."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))))
  (+ivy/project-search nil symbol "~/Dropbox/Notes/Work/Envato"))

(defun grass/search-all-notes (&optional symbol)
  "Conduct a text search across my work notes."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))))
  (+ivy/project-search nil symbol "~/Dropbox/Notes"))

;; Utils

(defun grass/today ()
  "Insert today's date"
  (format-time-string "%Y.%m.%d - %a"))

(defun grass/to-ascii-code (colour)
  "Convert a colour name to its ascii code"
  (cond
    ((string= colour "blue") "34")
    ((string= colour "red") "31")
    ((string= colour "yellow") "33")
    ((string= colour "green") "32")
    ((string= colour "cyan") "36")
    ((string= colour "magenta") "35")
    ((string= colour "black") "30")
    ((string= colour "white") "37")
    colour))


;; Comment box

(defun grass/point-is-in-comment-p ()
  "t if point is in comment or at the beginning of a commented line, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (looking-at "^\\s *\\s<")))

(defun grass/move-point-forward-out-of-comment ()
  "Move point forward until it's no longer in a comment"
  (while (grass/point-is-in-comment-p)
    (forward-char)))

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

;; Packages

;; This is actually provided by org
(use-package! org-mac-link
  :defer
  :commands org-mac-grab-link)

(use-package! git-link
  :defer
  :commands git-link git-link-commit
  :config
  (setq git-link-open-in-browser t))

;; Subtle highlight when switching buffers etc...
(use-package! beacon
  :init
  (setq beacon-color "#eaa427")
  (setq beacon-blink-when-window-scrolls nil)
  (beacon-mode 1))

(use-package! ox-pandoc
  :after org
  :init
  (setq org-pandoc-options-for-markdown '((atx-headers . t))
        org-pandoc-options-for-markdown_mmd '((atx-headers . t))
        org-pandoc-options-for-markdown_github '((atx-headers . t)))
  (with-eval-after-load 'org (require 'ox-pandoc)))

(use-package! ox-slack
  :after org
  :commands org-slack-export-as-slack)

;; See which commands I use
(use-package! keyfreq
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

(use-package! crux
  :defer
  :commands (crux-indent-defun crux-cleanup-buffer-or-region crux-move-beginning-of-line)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))


;; Secrets
(load "~/.emacs.secrets" t)

(after! lsp
  (setq lsp-clients-elixir-server-executable "~/dev/elixir-ls/rel/language_server.sh"))


;; My keybinds

(map! :leader
      :desc "Switch to last useful buffer" "`" #'grass/switch-to-previous-buffer
      (:prefix ("k" . "Grass keybinds")
       "w" #'grass/open-work-log
       "n" #'grass/find-notes

       (:prefix ("e" . "Edit")
        "f" 'crux-indent-defun
        "i" 'crux-cleanup-buffer-or-region
        "w" 'whitespace-cleanup)

       (:prefix ("s" . "Search")
        "w" #'grass/search-work-notes
        "n" #'grass/search-all-notes)

       (:prefix ("G" . "Git")
        "l" #'git-link
        "c" #'git-link-commit)))

(after! org
  (map! :leader
        "kg" #'org-mac-grab-link
        "kS" #'org-slack-export-as-slack))

(after! dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (define-key dired-mode-map [return] 'dired-find-alternate-file)
  (map!
   :n "-" #'dired-jump))
