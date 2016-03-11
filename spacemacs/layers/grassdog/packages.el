;;; packages.el --- grassdog Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq grassdog-packages
  '( ag
     string-inflection
     browse-kill-ring
     dired-single
     dired-filter
     peep-dired
     htmlize
     ox-reveal
     org-mac-link
     seeing-is-believing
     simpleclip
     char-menu
     nlinum
     keyfreq
     git-timemachine
     ))

;; List of packages to exclude.
(setq grassdog-excluded-packages '())

(defun grassdog/init-dired-filter ()
  (use-package dired-filter
    :commands (dired-filter-by-dot-files
                dired-filter-by-regexp
                dired-filter-pop)))

(defun grassdog/init-dired-single ()
  (use-package dired-single))

;;preview files in dired
(defun grassdog/init-peep-dired ()
  (use-package peep-dired
    :defer t
    :bind (:map dired-mode-map
            ("P" . peep-dired))))

(defun grassdog/init-ag ()
  (use-package ag
    :commands ag-project))

;; Create reveal js presentations in org mode.
(defun grassdog/init-ox-reveal ()
  (use-package ox-reveal
    :init
    (progn
      (setq org-reveal-root (concat "file://" (expand-file-name "~/Dropbox/Backups/Reveal/reveal.js"))))))

;; Yank links to Mac documents
(defun grassdog/init-org-mac-link ()
  (use-package org-mac-link
    :commands org-mac-grab-link))

(defun grassdog/init-browse-kill-ring ()
  (use-package browse-kill-ring
    :commands browse-kill-ring))

(defun grassdog/init-string-inflection ()
  (use-package string-inflection
    :commands (string-inflection-cycle
                string-inflection-underscore
                string-inflection-upcase
                string-inflection-lower-camelcase
                string-inflection-camelcase
                string-inflection-lisp)))

;; Note that htmlize is already initialized elsewhere

(defun grassdog/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :init
    (progn
      ; (setq seeing-is-believing-prefix "C-c x")
      (spacemacs|diminish seeing-is-believing " b" " b")
      (spacemacs/declare-prefix-for-mode 'ruby-mode "md" "debugging")
      (evil-leader/set-key-for-mode 'ruby-mode
        "ds" 'seeing-is-believing-run
        "dx" 'seeing-is-believing-run-as-xmpfilter
        "dc" 'seeing-is-believing-clear)
      (add-hook 'ruby-mode-hook 'seeing-is-believing))))

;; Keep system clipboard separate from kill ring
(defun grassdog/init-simpleclip ()
  (use-package simpleclip
    :init
    (simpleclip-mode 1)))

(defun grassdog/init-char-menu ()
  (use-package char-menu
    ; Em-dash is first
    :config (setq char-menu '("—" "‘’" "“”" "…" "«»" "–"
                            ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                            ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
                            ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")))))

;; Keep system clipboard separate from kill ring
(defun grassdog/init-nlinum ()
  (use-package nlinum
    :init
    (setq nlinum-format "%4d ")

    ;; Line numbers for coding please
    (add-hook 'prog-mode-hook
      (lambda ()
        (nlinum-mode 1)))))

;;;;;;;;;;;;;;;;;;;
;; Key Frequency ;;
;;;;;;;;;;;;;;;;;;;

(defun grassdog/init-keyfreq ()
  (use-package keyfreq
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
    (keyfreq-autosave-mode 1)))


(defun grassdog/init-git-timemachine ()
  (use-package git-timemachine
    :commands git-timemachine))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
