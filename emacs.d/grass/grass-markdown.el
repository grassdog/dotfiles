
(use-package markdown-mode
  :mode (("\\.markdown\\'"    . markdown-mode)
         ("\\.md\\'"    . markdown-mode))
  :config
  (setq-default markdown-command "pandoc -S -s --self-contained -f markdown -t html5 ")

  (defun grass/markdown-enter-key-dwim ()
    "If in a list enter a new list item, otherwise insert enter key as normal."
    (interactive)
    (let ((bounds (markdown-cur-list-item-bounds)))
      (if bounds
          ;; In a list
          (call-interactively #'markdown-insert-list-item)
        ;; Not in a list
        (markdown-enter-key))))

  (define-key markdown-mode-map (kbd "RET") 'grass/markdown-enter-key-dwim)

  (setq markdown-imenu-generic-expression
        '(("title"  "^\\(.*\\)[\n]=+$" 1)
          ("h2-"    "^\\(.*\\)[\n]-+$" 1)
          ("h1"   "^# \\(.*\\)$" 1)
          ("h2"   "^## \\(.*\\)$" 1)
          ("h3"   "^### \\(.*\\)$" 1)
          ("h4"   "^#### \\(.*\\)$" 1)
          ("h5"   "^##### \\(.*\\)$" 1)
          ("h6"   "^###### \\(.*\\)$" 1)
          ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

  (use-package pandoc-mode
    :diminish pandoc-mode)

  (add-hook 'markdown-mode-hook
      (lambda ()
        ;; Remove for now as they interfere with indentation
        ;; (define-key yas-minor-mode-map [(tab)] nil)
        ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
        (setq imenu-generic-expression markdown-imenu-generic-expression)))

  (add-hook 'markdown-mode-hook 'pandoc-mode)

  ;; Preview markdown file in Marked.app
  (defun grass/markdown-open-marked ()
    "run Marked.app on the current file and revert the buffer"
    (interactive)
    (shell-command
     (format "open -a 'Marked 2' %s"
             (shell-quote-argument (buffer-file-name))))))

(provide 'grass-markdown)
