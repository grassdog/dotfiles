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
(require 'solarized-theme)

;; GUI Mode settings
(when (display-graphic-p)
      (load-theme 'solarized-dark t)
      ;; Highlight the current line
      (global-hl-line-mode +1))

(provide 'grass-ui)
