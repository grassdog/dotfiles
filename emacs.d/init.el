
;; File paths
(defvar grass-root-dir (file-name-directory load-file-name)
  "The root dir of my Emacs config.")
(defvar grass-history-dir (expand-file-name "history" grass-root-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar grass-config-dir (expand-file-name "grass" grass-root-dir)
  "The directory containing configuration files.")
(defvar grass-snippets-dir (expand-file-name "snippets" grass-root-dir)
  "A house for snippets.")
(defvar grass-modules-dir (expand-file-name "modules" grass-root-dir)
  "Modules directory.")

;; Ensure history directory exists
(unless (file-exists-p grass-history-dir)
  (make-directory grass-history-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up load paths
(add-to-list 'load-path grass-config-dir)
(add-to-list 'load-path grass-modules-dir)

(require 'grass-packages)
(require 'grass-ui)
(require 'grass-editor)
(require 'grass-extensions)
(require 'grass-keybindings)

;; Fix our shell environment on OSX
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

(message "Roll on.")
