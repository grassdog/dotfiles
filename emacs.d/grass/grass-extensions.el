
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

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

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
