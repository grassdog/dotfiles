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
  '(
     ag
     string-inflection
     ignoramous
     browse-kill-ring
     dired-single
     htmlize
     ox-reveal
     org-mac-grab-link
    ))

;; List of packages to exclude.
(setq grassdog-excluded-packages '())

(defun grassdog/init-dired-single ()
  (use-package dired-single))

(defun grassdog/init-ag ()
  (use-package ag
    :commands ag-project))


(defun grass/init-ox-reveal ()
  ;; Create reveal js presentations in org mode.
  (use-package ox-reveal
    :init
    (progn
      (setq org-reveal-root (concat "file://" (expand-file-name "~/Dropbox/Backups/Reveal/reveal.js"))))))


(defun grass/init-org-mac-grab-link ()
  (use-package org-mac-link))

(defun grass/init-ignoramus ()
  ;; Ignore certain files
  (use-package ignoramus
    :init
    (ignoramus-setup '(comint completions grep ido
                       nav pcomplete projectile speedbar vc))))

(defun grass/init-browse-kill-ring ()
  (use-package browse-kill-ring))

(defun grass/init-string-inflection ()
  (use-package string-inflection))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
