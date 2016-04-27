;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
      ;; ----------------------------------------------------------------
      ;; Example of useful layers you may want to use right away.
      ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
      ;; <M-m f e R> (Emacs style) to install them.
      ;; ----------------------------------------------------------------
      (auto-completion
        :variables
        auto-completion-return-key-behavior 'complete
        auto-completion-tab-key-behavior 'complete
        auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
        auto-completion-enable-snippets-in-popup t
        :disabled-for org erc markdown
        )
      better-defaults
      colors
      emacs-lisp
      erc
      git
      github
      markdown
      org
      (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
      (spell-checking :variables
                      spell-checking-enable-by-default nil
                      spell-checking-enable-auto-dictionary nil)
      syntax-checking
      version-control

      clojure
      dash
      dockerfile

      elixir
      elm
      evil-snipe
      evil-commentary

      evil-little-word

      finance

      haskell
      html
      ibuffer
      javascript
      pandoc
      osx

      react
      (ruby :variables ruby-version-manager 'chruby)
      ruby-on-rails
      rust
      shell-scripts
      typescript
      vagrant
      yaml

      grassdog
    )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(emmet-mode)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; Deets
  (setq user-full-name "Ray Grasso"
    user-mail-address "ray.grasso@gmail.com")

  ;; Spacemacs
  (setq powerline-default-separator 'arrow)

  ;; Some key hydras
  (defhydra hydra-window ()
    "
Movement^^      ^Resize^     ^Split^
--------------------------------------------
_h_ ←           _q_ X←      _v_ertical
_j_ ↓           _w_ X↓      _x_ horizontal
_k_ ↑           _e_ X↑
_l_ →           _r_ X→      _o_nly this
_SPC_ cancel    _d_elete
"
    ("h" windmove-left nil)
    ("j" windmove-down nil)
    ("k" windmove-up nil)
    ("l" windmove-right nil)
    ("q" grass/move-splitter-left nil)
    ("w" grass/move-splitter-down nil)
    ("e" grass/move-splitter-up nil)
    ("r" grass/move-splitter-right nil)
    ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
    nil)
    ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
    nil)
    ("d" delete-window nil)
    ("o" delete-other-windows nil)
    ("SPC" nil nil))


  (defhydra hydra-string-case ()
    "word case"
    ("c" capitalize-word "Capitalize")
    ("u" upcase-word "UPPER")
    ("l" downcase-word "lower")
    ("s" string-inflection-underscore "lower_snake")
    ("n" string-inflection-upcase "UPPER_SNAKE")
    ("a" string-inflection-lower-camelcase "lowerCamel")
    ("m" string-inflection-camelcase "UpperCamel")
    ("d" string-inflection-lisp "dash-case"))

  ;; Add prefix names to some missing core ones
  (spacemacs/declare-prefix "d" "dash")
  (spacemacs/declare-prefix "P" "pandoc")

  ;; My evil bindings
  (spacemacs/declare-prefix "o" "personal")

  (spacemacs/declare-prefix "ou" "utilities")
  (evil-leader/set-key "out" 'display-time-world)
  (evil-leader/set-key "ouc" 'quick-calc)
  (evil-leader/set-key "ouu" 'browse-url)
  (evil-leader/set-key "our" 'grass/rename-file-and-buffer)
  (evil-leader/set-key "oub" 'grass/comment-box)
  (evil-leader/set-key "ou!" 'grass/shell-command-with-prefix-arg)

  (spacemacs/declare-prefix "os" "search-replace")
  (evil-leader/set-key "osa" 'ag-project)
  (evil-leader/set-key "osr" 'grass/replace-string)
  (evil-leader/set-key "osR" 'grass/replace-regexp)
  (evil-leader/set-key "osq" 'grass/query-replace-string)
  (evil-leader/set-key "osQ" 'grass/query-replace-regexp)
  (evil-leader/set-key "osf" 'isearch-forward-regexp)
  (evil-leader/set-key "osb" 'isearch-reverse-regexp)

  (spacemacs/declare-prefix "of" "formatting")
  (evil-leader/set-key "off" 'grass/indent-region-or-buffer)
  (evil-leader/set-key "ofw" 'whitespace-cleanup)
  (evil-leader/set-key "ofb" 'web-beautify-js)
  (evil-leader/set-key "ofj" 'json-pretty-print-buffer)
  (evil-leader/set-key "ofh" 'web-beautify-html)
  (evil-leader/set-key "ofc" 'web-beautify-css)

  (spacemacs/declare-prefix "oe" "editing")
  (evil-leader/set-key "oey" 'browse-kill-ring)
  (evil-leader/set-key "oee" 'yas-expand)
  (evil-leader/set-key "oei" 'hydra-string-case/body)

  (global-set-key (kbd "C-, ~") 'hydra-string-case/body)

  (evil-leader/set-key "ow" 'hydra-window/body)

  (global-set-key (kbd "C-, w") 'hydra-window/body)

  (spacemacs/declare-prefix "ob" "bookmarks")
  (evil-leader/set-key "obc" 'grass/open-cheats)
  (evil-leader/set-key "obw" 'grass/open-work-log)
  (evil-leader/set-key "obs" 'grass/open-sideproject-log)
  (evil-leader/set-key "obn" 'grass/find-notes)

  (evil-leader/set-key "oc" 'char-menu)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Backtab to unindent
  (define-key global-map [backtab] 'evil-shift-left-line)

  ;; Easier window switching
  (global-set-key [S-left] 'windmove-left)
  (global-set-key [S-right] 'windmove-right)
  (global-set-key [S-up] 'windmove-up)
  (global-set-key [S-down] 'windmove-down)

  ;; Old muscle memory
  (global-set-key (kbd "C-, C-,") 'spacemacs/alternate-buffer)
  (global-set-key (kbd "C-, p") 'helm-projectile-find-file)
  (global-set-key (kbd "C-, C-p") 'helm-projectile-find-file)
  (global-set-key (kbd "C-, r") 'helm-recentf)
  (global-set-key (kbd "C-, o") 'helm-mini)
  (global-set-key (kbd "C-, e") 'yas-expand)
  (global-set-key (kbd "s-e") 'yas-expand)

  ;; Utilities
  (global-set-key (kbd "C-, u t") 'display-time-world)
  (global-set-key (kbd "C-, u b") 'grass/comment-box)
  (global-set-key (kbd "C-, u u") 'browse-url)

  ;; Shell
  (global-set-key (kbd "C-, !") 'grass/shell-command-with-prefix-arg)

  (global-set-key (kbd "C-, g") 'magit-status)

  ;; Move text
  (global-set-key (kbd "<C-S-up>") 'move-text-up)
  (global-set-key (kbd "<C-S-down>") 'move-text-down)

  ;; Cleanup
  (global-set-key (kbd "C-, f") 'grass/indent-region-or-buffer)
  (define-key global-map (kbd "C-, w") 'whitespace-cleanup)

  ;; Search and replace
  (global-set-key (kbd "C-, s r") 'grass/replace-string)
  (global-set-key (kbd "C-, s R") 'grass/replace-regexp)
  (global-set-key (kbd "C-, s q") 'grass/query-replace-string)
  (global-set-key (kbd "C-, s Q") 'grass/query-replace-regexp)
  (global-set-key (kbd "C-, s f") 'isearch-forward-regexp)
  (global-set-key (kbd "C-, s b") 'isearch-reverse-regexp)

  (global-set-key (kbd "C-, s a") 'ag-project)

  (global-set-key (kbd "C-, y") 'browse-kill-ring)
  (global-set-key (kbd "C-, C-i") 'string-inflection-cycle)

  ;; Quick buffer changing
  (global-set-key [s-up] 'dired-jump)
  (global-set-key (kbd "s-}") 'next-buffer)
  (global-set-key (kbd "s-{") 'previous-buffer)

  (global-set-key (kbd "C-, c") 'char-menu)

  (add-hook 'auto-highlight-symbol-mode-hook
    (lambda ()
      (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
      (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
      (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
      (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)
      (define-key auto-highlight-symbol-mode-map (kbd "M-<up>") 'ahs-backward)
      (define-key auto-highlight-symbol-mode-map (kbd "M-<down>") 'ahs-forward)))

  (with-eval-after-load 'smartparens
    (defun grass/smartparens-ruby-brace (id action context)
      (spacemacs/smartparens-pair-newline-and-indent id action context)
      (previous-line)
      (end-of-line))

    (sp-local-pair 'ruby-mode "{" nil :post-handlers
      '((grass/smartparens-ruby-brace "RET")))

    (bind-keys
      :map smartparens-mode-map

      ("<s-right>" . sp-forward-slurp-sexp)
      ("<s-left>" . sp-forward-barf-sexp)
      ("<C-s-left>" . sp-backward-slurp-sexp)
      ("<C-s-right>" . sp-backward-barf-sexp)))


  ;; Clear prefix binding
  (add-hook 'css-mode-hook
    (lambda ()
      (rainbow-mode 1)))

  ;; Clear prefix binding
  (add-hook 'flyspell-mode-hook
    (lambda ()
      (define-key flyspell-mode-map (kbd "C-,") nil)))

  ;; Markdown
  (add-hook 'markdown-mode-hook
    (lambda ()
      (defun grass/markdown-enter-key-dwim ()
        "If in a list enter a new list item, otherwise insert enter key as normal."
        (interactive)
        (let ((bounds (markdown-cur-list-item-bounds)))
          (if bounds
            ;; In a list
            (call-interactively #'markdown-insert-list-item)
            ;; Not in a list
            (markdown-enter-key))))

      (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
        "p" 'grass/markdown-open-in-marked-app)

      (sp-local-pair 'markdown-mode "`" nil :actions nil)

      (define-key markdown-mode-map (kbd "RET") 'grass/markdown-enter-key-dwim)))

  ;;;;;;;;;;;;;;;
  ;; Proselint ;;
  ;;;;;;;;;;;;;;;

  (with-eval-after-load 'flycheck
    (flycheck-define-checker proselint
      "A linter for prose."
      :command ("proselint" source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": "
                (id (one-or-more (not (any " "))))
                (message (one-or-more not-newline)
                        (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                line-end))
      :modes (text-mode markdown-mode gfm-mode org-mode))
    (add-to-list 'flycheck-checkers 'proselint))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo-keyword-faces
 (quote
   (("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXX" . "#cc9393")
     ("XXXX" . "#cc9393")
     ("???" . "#cc9393")
     ("DEBUG" . "#8c5353")
     ("GRASS" . "#dc8cc3")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
