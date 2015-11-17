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
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)
    (setq company-global-modes
      '(not markdown-mode org-mode erc-mode))

    (define-key company-active-map [escape] 'company-abort)
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

  (use-package flycheck-tip
    :bind ("C-, C-n" . flycheck-tip-cycle))

  (use-package flycheck-pos-tip
    :disabled
    :init
    (eval-after-load 'flycheck
      '(setq flycheck-display-errors-function
            #'flycheck-pos-tip-error-messages))))


(use-package magit
  :bind ("C-, g" . magit-status))

;; Line numbers for coding please
(setq on-console (null window-system))
(setq linum-format (if on-console "%4d " "%4d"))

(use-package string-inflection
  :bind ("C-, C-u" . string-inflection-cycle))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (add-hook 'enh-ruby-mode-hook #'smartparens-mode)
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'scheme-mode-hook #'smartparens-mode))

;; Wrapping
(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(bind-keys
 :map smartparens-mode-map
 ("C-, ] ("  . wrap-with-parens)
 ("C-, ] ["  . wrap-with-brackets)
 ("C-, ] {"  . wrap-with-braces)
 ("C-, ] '"  . wrap-with-single-quotes)
 ("C-, ] \"" . wrap-with-double-quotes)
 ("C-, ] _"  . wrap-with-underscores)
 ("C-, ] `"  . wrap-with-back-quotes))

(use-package hideshow
  :diminish hs-minor-mode)

;;;;;;;;;;;;;
;; Alignment
;;;;;;;;;;;;;

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (align-repeat start end ,regexp ,justify-right after)))))

(create-align-repeat-x "comma" "," nil t)
(create-align-repeat-x "semicolon" ";" nil t)
(create-align-repeat-x "colon" ":" nil t)
(create-align-repeat-x "equal" "=")
(create-align-repeat-x "hash" "=>")
(create-align-repeat-x "math-oper" "[+\\-*/]")
(create-align-repeat-x "ampersand" "&")
(create-align-repeat-x "bar" "|")
(create-align-repeat-x "left-paren" "(")
(create-align-repeat-x "right-paren" ")" t)

;; Bindings
(global-set-key (kbd "C-, a a") 'align)
(global-set-key (kbd "C-, a r") 'align-repeat)
(global-set-key (kbd "C-, a m") 'align-repeat-math-oper)
(global-set-key (kbd "C-, a .") 'align-repeat-decimal)
(global-set-key (kbd "C-, a ,") 'align-repeat-comma)
(global-set-key (kbd "C-, a ;") 'align-repeat-semicolon)
(global-set-key (kbd "C-, a :") 'align-repeat-colon)
(global-set-key (kbd "C-, a =") 'align-repeat-equal)
(global-set-key (kbd "C-, a >") 'align-repeat-hash)
(global-set-key (kbd "C-, a &") 'align-repeat-ampersand)
(global-set-key (kbd "C-, a |") 'align-repeat-bar)
(global-set-key (kbd "C-, a (") 'align-repeat-left-paren)
(global-set-key (kbd "C-, a )") 'align-repeat-right-paren)

(add-hook 'prog-mode-hook
  (lambda ()
    (linum-mode)
    (hs-minor-mode t)
    ;; Treat underscore as a word character
    (modify-syntax-entry ?_ "w")))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package auto-highlight-symbol
  :defer t
  :init
    (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)
  :config
    '(ahs-case-fold-search nil)
    '(ahs-default-range (quote ahs-range-whole-buffer)))

;; Show current function in modeline
(which-function-mode)

(set-default 'imenu-auto-rescan t)

(global-set-key (kbd "C-, i") 'imenu)

(use-package puppet-mode
  :defer t)

(use-package powershell
  :mode  (("\\.ps1$" . powershell-mode)
          ("\\.psm$" . powershell-mode)))

(use-package rust-mode
  :defer t)

(use-package python
  :defer t)

(provide 'grass-coding)
