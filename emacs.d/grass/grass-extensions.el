
(defun grass-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory grass-root-dir 0))

;; Quick buffer switch
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(evil-leader/set-key "," 'switch-to-previous-buffer)

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
