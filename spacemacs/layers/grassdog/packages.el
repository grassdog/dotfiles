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
     ;; ignoramous
     browse-kill-ring
     dired-single
     dired-filter
     htmlize
     ox-reveal
     org-mac-link
     ))

;; List of packages to exclude.
(setq grassdog-excluded-packages '())

(defun grassdog/init-dired-filter ()
  (use-package dired-filter
    :bind (("C-, d d" . dired-filter-by-dot-files)
           ("C-, d r" . dired-filter-by-regexp)
           ("C-, d p" . dired-filter-pop)
           ("C-, d p" . dired-filter-pop))))

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

;; ;; Ignore certain files
;; (defun grassdog/init-ignoramus ()
;;   (use-package ignoramus
;;     :init
;;     (ignoramus-setup '(comint completions grep ido
;;                         nav pcomplete projectile speedbar vc))))

(defun grassdog/init-browse-kill-ring ()
  (use-package browse-kill-ring
    :commands browse-kill-ring))

(defun grassdog/init-string-inflection ()
  (use-package string-inflection
    :defer nil
    :commands string-inflection-cycle))

;; Note that htmlize is already initialized elsewhere

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
