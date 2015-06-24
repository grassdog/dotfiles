
(defun grass/recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory grass/dotfiles-dir 0))

;; Quick buffer switch
(defun grass/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Switch buffers, even in non evil modes
(evil-leader/set-key "," 'grass/switch-to-previous-buffer)
(global-set-key (kbd "C-, ,") 'grass/switch-to-previous-buffer)

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

(defun grass/move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun grass/move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; Common files

(defun grass/open-cheats ()
  "Open Emacs cheats file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs.md"))

(defun grass/open-reboot ()
  "Open Emacs reboot org file"
  (interactive)
  (find-file "~/Dropbox/Notes/Emacs Reboot.org"))

(provide 'grass-extensions)
