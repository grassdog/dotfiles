
;; File paths
(defvar grass-dotfiles-dir (file-name-directory load-file-name)
  "The root dir of my Emacs config.")
(defvar grass-config-dir (expand-file-name "grass" grass-dotfiles-dir)
  "The directory containing configuration files.")
(defvar grass-snippets-dir (expand-file-name "snippets" grass-dotfiles-dir)
  "A house for snippets.")
(defvar grass-savefile-dir (expand-file-name "savefile" grass-dotfiles-dir)
  "This folder stores all the automatically generated save/history-files.")

;; Ensure savefile directory exists
(unless (file-exists-p grass-savefile-dir)
  (make-directory grass-savefile-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up load paths
(add-to-list 'load-path grass-config-dir)

(require 'grass-packages)
(require 'grass-ui)
(require 'grass-editor)
(require 'grass-extensions)
(require 'grass-keybindings)
(require 'grass-helm)
(require 'grass-projectile)
(require 'grass-orgmode)
(require 'grass-osx)

(message "Roll on.")
