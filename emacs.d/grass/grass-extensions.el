
(defun grass-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory grass-root-dir 0))

;; Common files

(defun grass-open-cheats ()
  "Open Emacs cheats file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs.md"))

(defun grass-open-reboot ()
  "Open Emacs reboot org file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs Reboot.org"))

(provide 'grass-extensions)
