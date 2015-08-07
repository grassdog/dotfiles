
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'"    . markdown-mode)
         ("\\.md\\'"    . markdown-mode))
  :config
  (setq-default markdown-command "pandoc -S -s --self-contained -f markdown -t html5 ")

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

  :init
  (add-hook 'markdown-mode-hook
      (lambda ()
        ;; Remove for now as they interfere with indentation
        (define-key yas-minor-mode-map [(tab)] nil)
        (define-key yas-minor-mode-map (kbd "TAB") nil)
        (global-set-key (kbd "C-, l") 'markdown-preview-file)
        (setq imenu-generic-expression markdown-imenu-generic-expression)))

  ;; Preview markdown file in Marked.app
  (defun markdown-preview-file ()
    "run Marked.app on the current file and revert the buffer"
    (interactive)
    (shell-command
     (format "open -a 'Marked 2' %s"
             (shell-quote-argument (buffer-file-name))))))

(provide 'grass-markdown)
