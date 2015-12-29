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
     htmlize
     ox-reveal
     org-mac-link
     seeing-is-believing
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
    :defer nil
    :commands string-inflection-cycle))

;; Note that htmlize is already initialized elsewhere

(defun grassdog/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :commands (seeing-is-believing-run
               seeing-is-believing-run-as-xmpfilter
               seeing-is-believing-clear)
    :init
    ; (setq seeing-is-believing-prefix "C-c x")

    (spacemacs/declare-prefix-for-mode 'ruby-mode "md" "debugging")
    (evil-leader/set-key-for-mode 'ruby-mode
      "ds" 'seeing-is-believing-run
      "dx" 'seeing-is-believing-run-as-xmpfilter
      "dc" 'seeing-is-believing-clear)
    (add-hook 'ruby-mode-hook 'seeing-is-believing)))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
