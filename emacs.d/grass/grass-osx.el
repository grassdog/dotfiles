
;; Fix our shell environment on OSX
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  ;; Default font thanks
  (set-frame-font "Menlo-12"))

(provide 'grass-osx)
