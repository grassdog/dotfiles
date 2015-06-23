
;; Better suggestions
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(require 'ido-vertical-mode)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" grass/savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      org-completion-use-ido t)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;; Allow up and down arrow to work for navigation
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-vertical-mode 1)

;; Smarter fuzzy matching for ido
(flx-ido-mode +1)

;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

(global-set-key (kbd "M-x") 'helm-M-x)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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


(provide 'grass-ido)
