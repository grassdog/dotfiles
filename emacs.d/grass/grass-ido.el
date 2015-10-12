
(use-package ido
  :config
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
  :init
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode t)

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

    (use-package flx-ido
      :config
      ;; disable ido faces to see flx highlights
      (setq ido-use-faces nil)
      ;; Smarter fuzzy matching for ido
      (flx-ido-mode +1))

    (use-package ido-ubiquitous
      :disabled
      :init (ido-ubiquitous-mode))

    (use-package ido-vertical-mode
      :init
      (ido-vertical-mode 1)

      ;; Allow up and down arrow to work for navigation
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))))


(provide 'grass-ido)
