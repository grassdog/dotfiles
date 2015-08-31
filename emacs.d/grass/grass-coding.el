(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|XXX\\|HACK\\|DEBUG\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(use-package company
  :diminish company-mode
  :config
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 3)
    (setq company-global-modes
      '(not markdown-mode org-mode erc-mode))
    (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  :diminish yas-minor-mode
  :init

  (defun grass/do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas-expand)))

  (defun grass/check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun grass/tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      (indent-for-tab-command)
      (if (or (not yas-minor-mode)
              (null (grass/do-yas-expand)))
          (if (grass/check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (indent-for-tab-command)))))))))

  (defun grass/tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (grass/do-yas-expand)))
        (if company-candidates
            (company-complete-selection)
          (if (grass/check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (yas-next-field))))
            (yas-next-field)))))

  (defun grass/expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (grass/do-yas-expand))
            (company-abort))
        (company-complete-selection)))

  (defun grass/abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

  (setq yas-verbosity 1)
  (yas-global-mode 1)

  (global-set-key [tab] 'grass/tab-indent-or-complete)
  (global-set-key (kbd "TAB") 'grass/tab-indent-or-complete)
  (global-set-key [(control return)] 'company-complete-common)

  (define-key company-active-map [tab] 'grass/expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'grass/expand-snippet-or-complete-selection)

  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'grass/tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'grass/tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'grass/abort-company-or-yas)
  (define-key yas-minor-mode-map (kbd "C-, e") 'yas-expand))

(use-package web-beautify
  :commands (web-beautify-js web-beautify-css web-beautify-html))



(use-package flycheck
  :defer t
  :config
  (use-package flycheck-pos-tip))

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function
         #'flycheck-pos-tip-error-messages))

(use-package magit
  :bind ("C-, g" . magit-status))

;; Line numbers for coding please
(setq on-console (null window-system))
(setq linum-format (if on-console "%4d " "%4d"))

(use-package string-inflection
  :bind ("C-, C-u" . string-inflection-cycle))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (add-hook 'enh-ruby-mode-hook #'smartparens-mode)
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode))

(require 'hideshow)
(diminish 'hs-minor-mode)

;;Alignment
(add-hook 'prog-mode-hook
  (lambda ()
    (linum-mode 1)
    (hs-minor-mode t)
    (global-set-key (kbd "C-, a =")
      (lambda () (interactive)
        (grass/align-to-equals (region-beginning) (region-end))))

    (global-set-key (kbd "C-, a :")
      (lambda () (interactive)
        (grass/align-to-colon (region-beginning) (region-end))))))

;; Show current function in modeline
(which-function-mode)

(set-default 'imenu-auto-rescan t)

(global-set-key (kbd "C-, i") 'imenu)

(use-package puppet-mode
  :defer t)

(use-package rust-mode
  :defer t)

(use-package python
  :defer t)

(provide 'grass-coding)
