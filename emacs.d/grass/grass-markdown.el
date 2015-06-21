
(require 'markdown-mode)

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

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq imenu-generic-expression markdown-imenu-generic-expression)
            (flyspell-mode +1)))

;; Preview markdown file in Marked.app
(defun markdown-preview-file ()
  "run Marked.app on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name)))))

(global-set-key (kbd "C-c m") 'markdown-preview-file)

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(provide 'grass-markdown)
