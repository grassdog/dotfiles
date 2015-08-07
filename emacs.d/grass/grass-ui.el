;; UI config

;; Get rid of chrome
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No menu bar if we're in the console
(unless (display-graphic-p)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; No blinking cursor
(blink-cursor-mode -1)

;; No startup screen
(setq inhibit-startup-screen t)

;; No bell thanks
(setq ring-bell-function 'ignore)

; Text mode by default for scratch buffer
;(setq initial-major-mode 'text-mode)

;; Nice scrolling
(setq scroll-margin 4
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; A more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

(use-package tabbar
  :ensure t
  :init
  (setq tabbar-ruler-global-tabbar nil)
  (setq tabbar-ruler-global-ruler nil)
  (setq tabbar-ruler-popup-menu nil)
  (setq tabbar-ruler-popup-toolbar nil)
  (setq tabbar-ruler-popup-scrollbar nil)

  ;; Custom grouping code to try later
  ;; (defun tabbar-buffer-groups-by-dir ()
  ;;         "Put all files in the same directory into the same tab bar"
  ;;         (with-current-buffer (current-buffer)
  ;;           (let ((dir (expand-file-name default-directory)))
  ;;             (cond ;; assign group name until one clause succeeds, so the order is important
  ;;             ((eq major-mode 'dired-mode)
  ;;               (list "Dired"))
  ;;             ((memq major-mode
  ;;                     '(help-mode apropos-mode Info-mode Man-mode))
  ;;               (list "Help"))
  ;;             ((string-match-p "\*.*\*" (buffer-name))
  ;;               (list "Misc"))
  ;;             (t (list dir))))))

  ;; (defun tabbar-switch-grouping-method (&optional arg)
  ;;   "Changes grouping method of tabbar to grouping by dir.
  ;; With a prefix arg, changes to grouping by major mode."
  ;;   (interactive "P")
  ;;   (ignore-errors
  ;;     (if arg
  ;;       (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; the default setting
  ;;         (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))))

  (require 'tabbar-ruler)
  (tabbar-mode 1)
  (global-set-key (kbd "s-{") 'tabbar-backward-tab)
  (global-set-key (kbd "s-}") 'tabbar-forward-tab)
  (global-set-key (kbd "<s-right>") 'tabbar-forward-group)
  (global-set-key (kbd "<s-left>") 'tabbar-backward-group)
  (global-set-key (kbd "<s-up>") 'tabbar-press-home))



(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/shorten-modes nil)
  (setq sml/theme 'respectful)
  (sml/setup))

;; Ignore certain files
(use-package ignoramus
  :ensure t
  :init
  (ignoramus-setup))

(diminish 'abbrev-mode)

;; GUI Mode settings
(when (display-graphic-p)
  (use-package solarized
               :ensure solarized-theme
               :init (load-theme 'solarized-dark 'no-confirm)
               :config
               ;; Disable variable pitch fonts in Solarized theme
               (setq solarized-use-variable-pitch nil
                     ;; Don't add too much colours to the fringe
                     solarized-emphasize-indicators nil
                     ;; Keep font sizes the same
                     solarized-height-minus-1 1.0
                     solarized-height-plus-1 1.0
                     solarized-height-plus-2 1.0
                     solarized-height-plus-3 1.0
                     solarized-height-plus-4 1.0))
  ;; Highlight the current line
  (global-hl-line-mode +1))

(when (not (display-graphic-p))
  (use-package zenburn-theme
     :ensure t
     :init
     (load-theme 'zenburn 'no-confirm)))

(provide 'grass-ui)
