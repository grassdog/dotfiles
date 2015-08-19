;; Better suggestions
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" grass/savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      org-completion-use-ido t)

;; Bind `~` to go to homedir when in ido-find-file;
;; http://whattheemacsd.com/setup-ido.el-02.html
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

(require 'ido)
(ido-mode +1)

(use-package ido-ubiquitous
  :init
    (ido-ubiquitous-mode +1))

(use-package flx-ido
  :config
    ;; disable ido faces to see flx highlights
    (setq ido-use-faces nil)
  :init
    ;; Smarter fuzzy matching for ido
    (flx-ido-mode +1))

(require 'ido-vertical-mode)
(use-package ido-ubiquitous
  :config
    ;; Allow up and down arrow to work for navigation
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  :init
    (ido-vertical-mode 1))

(provide 'grass-ido)
