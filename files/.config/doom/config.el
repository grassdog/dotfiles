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

;; Give me some keybinds I like
(after! dired
  (map!
    :n "-" #'dired-jump))

;; Arrow key window movement
(after! evil!
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down))

;; Just quit thanks
(setq confirm-kill-emacs nil)

;; TODO Work out how to remove the previous bindings here and switch to "g"
(after! org
  (map! :map org-mode-map
        :localleader
        "G" #'org-mac-grab-link))


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



;; (setq grass/thirdleader-key ",")
;; TODO Map
;; "gl" 'git-link
;; "gc" 'git-link-commit
(map! :leader
      :desc "Switch to last useful buffer" "`"    #'grass/switch-to-previous-buffer)
