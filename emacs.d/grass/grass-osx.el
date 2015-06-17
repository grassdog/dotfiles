
;; Fix our shell environment on OSX
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  ;; Default font thanks
  (set-default-font "Menlo-12"))

(provide 'grass-osx)
