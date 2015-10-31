;; Indentation styles et al for all modes in one central location

;; Don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Default indentation
(setq-default tab-width 4)

;; Javascript
(setq-default js2-basic-offset 2)

;; JSON
(setq-default js-indent-level 2)

;; Sass
(setq css-indent-offset 2)

;; Coffeescript
(setq coffee-tab-width 2)

;; Python
(setq-default py-indent-offset 2)

;; XML
(setq-default nxml-child-indent 2)

;; Ruby
(setq ruby-indent-level 4)

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

(setq sentence-end-double-space nil)

;; Enforce proper whitespace
(require 'whitespace)
(diminish 'global-whitespace-mode)

(setq require-final-newline t)

;; Auto whitespace cleanup
(defgroup grass nil "Grass config."
  :group 'tools)

(defcustom grass/auto-whitespace-cleanup t
  "Non-nil means cleanup whitespace on save."
  :group 'grass
  :type 'boolean)

(defun grass/cleanup-whitespace-if-enabled ()
  (interactive)
  (when grass/auto-whitespace-cleanup
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'grass/cleanup-whitespace-if-enabled)

(defun grass/disable-whitespace-cleanup ()
  "Disabled whitespace-cleanup on save."
  (interactive)
  (setq grass/auto-whitespace-cleanup nil))

(defun grass/disable-whitespace-cleanup ()
  "Enables whitespace-cleanup on save."
  (interactive)
  (setq grass/auto-whitespace-cleanup t))

(define-key global-map (kbd "C-, w") 'whitespace-cleanup)


;; Only show bad whitespace (Ignore empty lines at start and end of buffer)
(setq whitespace-style '(face tabs trailing space-before-tab indentation space-after-tab))
(global-whitespace-mode t)

(provide 'grass-codestyle)
