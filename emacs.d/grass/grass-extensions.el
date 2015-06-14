
(defun grass-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory grass-root-dir 0))

(provide 'grass-extensions)
