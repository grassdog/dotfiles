
;; Fix our shell environment on OSX
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

(provide 'grass-osx)
