;; better defaults
(progn

  (defvar sd/theme 'apropospriate-dark)

  ;; startup stuff
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq initial-scratch-message "")
  (setq inhibit-startup-screen t)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-echo-area-message (user-real-login-name))

  ;; core GUI stuff
  (setq enable-recursive-minibuffers t)
  (setq ring-bell-function 'ignore)
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-face-font 'default "Menlo-14.0")

  ;; line numbers when programming
  (setq-default display-line-numbers-width 4)
  (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode +1)))

  ;; cursor
  (blink-cursor-mode 1)
  (setq-default cursor-type 'box)

  ;; dark mode stuff?
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; basic editing features
  (delete-selection-mode 1)
  (setq-default indent-tabs-mode nil)
  (define-key prog-mode-map (kbd "RET") 'newline-and-indent)
  (show-paren-mode -1)
  (setq-default truncate-lines t)
  (global-hl-line-mode 1)

  ;; scrolling
  (pixel-scroll-mode 0) ;; not there yet
  (setq scroll-conservatively 101)
  (setq auto-window-vscroll nil)
  (setq scroll-step 1)

  ;; join-line
  (defun sd/join-line () (interactive) (join-line 1))
  (global-set-key (kbd "M-k") #'sd/join-line)

  ;; dont wrap lines when editing plain text
  (add-hook 'text-mode-hook 'toggle-word-wrap)
  (add-hook 'text-mode-hook 'toggle-truncate-lines)

  ;; whitespace in files
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq require-final-newline t)

  ;; prefer to split windows side by side
  (setq split-height-threshold nil)
  (setq split-width-threshold 125)

  ;; "customize"
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;; disable backup, auto-save, and lock files
  (setq-default backup-inhibited t)
  (setq-default auto-save-default nil)
  (setq create-lockfiles nil)

  ;; fix $PATH
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))) ;; for dired / eshell
  (setq exec-path (append '("/usr/local/bin") exec-path)) ;; for js-comint

  ;; utf-8
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; ;; not sure what this does... (TODO)
  ;; (set-display-table-slot standard-display-table 0 ?~)

  ;; disable useless shortcuts
  (global-unset-key (kbd "C-z")) ;; stop minimizing
  (global-unset-key (kbd "s-p")) ;; stop asking to print
  (global-unset-key (kbd "s-k")) ;; don't destroy current buffer
  )




;; bare-bones theme management
(progn
  ;; favorite themes listed here in rough priority of preference
  ;; try them out with C-x C-e while cursor's at end of line

  ;; (counsel-load-theme-action "apropospriate-dark")
  ;; (counsel-load-theme-action "atom-one-dark")
  ;; (counsel-load-theme-action "solarized-dark")
  ;; (counsel-load-theme-action "zenburn")
  ;; (counsel-load-theme-action "spacemacs-dark")
  ;; (counsel-load-theme-action "sanityinc-tomorrow-eighties")
  ;; (counsel-load-theme-action "sanityinc-tomorrow-night")
  ;; (counsel-load-theme-action "monokai")
  ;; (counsel-load-theme-action "monokai-pro")
  ;; (counsel-load-theme-action "monokai-alt")
  ;; (counsel-load-theme-action "naquadah")

  (defun sd/paste-current-theme ()
    (interactive)
    (insert
     (symbol-name
      (car custom-enabled-themes))))

  (defun sd/maybe-load-theme (theme)
    (when (member theme (custom-available-themes))
      (load-theme theme t t)
      (enable-theme theme))))




;; bare-bones package management
(progn
  (require 'package)
  (package-initialize)

  ;; load theme as early as possible in typical case
  (sd/maybe-load-theme sd/theme)

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)

  (defmacro sd/ensure-packages (&rest pkgs)
    (dolist (pkg pkgs)
      (unless (package-installed-p pkg)
        (package-install pkg))))

  (sd/ensure-packages

   ;; themes
   apropospriate-theme
   zenburn-theme
   spacemacs-theme
   color-theme-sanityinc-tomorrow
   atom-one-dark-theme
   one-themes
   solarized-theme
   color-theme-sanityinc-solarized
   monokai-theme
   monokai-pro-theme
   monokai-alt-theme
   naquadah-theme

   ;; editing helpers
   undo-tree
   multiple-cursors
   rainbow-delimiters
   paredit
   hl-todo
   default-text-scale

   ;; buffer management
   buffer-move
   windsize
   dimmer

   ;; javascript
   js-comint
   ;; ts-comint

   ;; markdown
   markdown-mode
   polymode
   poly-markdown

   ;; git
   magit

   ;; fuzzy-completion
   counsel
   smex
   ivy
   )

  ;; definitely should be loaded by now
  (sd/maybe-load-theme sd/theme))




;; buffer management
(progn
  ;; dim inactive windows
  (dimmer-mode 1)
  (setq dimmer-fraction 0.3)
  (add-hook 'buffer-list-update-hook 'dimmer-command-hook)

  ;; switch buffers with cmd-[  cmd-]
  (global-set-key (kbd "s-[") 'previous-buffer)
  (global-set-key (kbd "s-]") 'next-buffer)

  ;; switch windows with cmd-shift-[  cmd-shift-]
  (global-set-key (kbd "s-{") '(lambda () (interactive) (other-window -1)))
  (global-set-key (kbd "s-}") 'other-window)

  ;; swap buffers with ctrl-cmd-{arrows}
  (global-set-key (kbd "<C-s-up>")     'buf-move-up)
  (global-set-key (kbd "<C-s-down>")   'buf-move-down)
  (global-set-key (kbd "<C-s-left>")   'buf-move-left)
  (global-set-key (kbd "<C-s-right>")  'buf-move-right)

  ;; resize buffers via ctrl-shift-{arrows}
  (require 'windsize)
  (windsize-default-keybindings)

  ;; new blank buffers in current window with cmd-shift-n
  (defun sd/new-blank-bufer ()
    (interactive)
    (switch-to-buffer (generate-new-buffer-name "untitled"))
    (text-mode))
  (global-set-key (kbd "s-N") 'sd/new-blank-bufer)

  ;; unique buffer names
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t))




;; futuristic undo
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "s-z") #'undo-tree-undo)
(global-set-key (kbd "s-Z") #'undo-tree-redo)




;; futuristic git ui
(require 'magit)
(setq vc-follow-symlinks t)
(add-to-list 'magit-no-confirm 'stage-all-changes)
(add-to-list 'magit-no-confirm 'unstage-all-changes)
(global-set-key (kbd "s-G") #'magit-status)
(setq magit-section-initial-visibility-alist '((stashes . show)
                                               (unpushed . show)))



;; edit multiple lines
(require 'multiple-cursors)
(global-set-key (kbd "C-<return>") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-s->") 'mc/mark-next-like-this-symbol)
(define-key mc/keymap (kbd "<return>") nil)




;; mode line
(setq-default
 mode-line-format
 '("%e" (:eval (list " %* " mode-line-buffer-identification
                     " (%o) "
                     " %4l : %2c   "
                     " " (abbreviate-file-name default-directory)
                     " " mode-line-modes
                     " " mode-line-misc-info))))




;; rainbow-delimiters everywhere
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)




;; dired
(require 'dired-x)
(setq-default dired-auto-revert-buffer t)
(setq-default dired-use-ls-dired nil)
(setq dired-listing-switches "-alh")




;; global text adjustement
(default-text-scale-mode +1)




;; fuzzy-matching everywhere
(progn
  (require 'ivy)
  (require 'counsel)

  ;; show current position in list too
  (setq ivy-count-format "%d/%d ")

  ;; magit + ivy
  (setq magit-completing-read-function 'ivy-completing-read)

  ;; don't add ^ to beginning of *any* searches
  (setq ivy-initial-inputs-alist nil)

  ;; stop backspace closing minibuffer
  (setq ivy-on-del-error-function nil)

  ;; completion using C-M-i, *way* better than company-mode
  (setq completion-in-region-function 'ivy-completion-in-region)

  ;; ivy/swiper fuzzy matching
  (setq ivy-re-builders-alist
        '((swiper           . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (t                . ivy--regex-fuzzy)))

  (global-set-key (kbd "M-x")     'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-f")     'swiper)
  (global-set-key (kbd "<f1> f")  'counsel-describe-function)
  (global-set-key (kbd "<f1> v")  'counsel-describe-variable)
  (global-set-key (kbd "C-x b")   'ivy-switch-buffer)
  (global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)

  ;; easy way to clean up old buffers
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-switch-buffer-kill)

  ;; this always quits out of ivy-switch-buffer after ivy-switch-buffer-kill
  (add-to-list 'ivy-ignore-buffers "\*Backtrace\*")

  ;; make ivy more like ido for files/dirs
  (setq ivy-extra-directories nil) ;; no . or ..
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  ;; make ivy work /everywhere/
  (ivy-mode 1))




;; paredit
(progn
  (autoload 'enable-paredit-mode "paredit" nil t)
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode))




;; sane comment functionality
;; (currently depends on paredit)
(defun sd/comment-dwim ()
  (interactive)
  (if (or (region-active-p)
          (string-blank-p
           (buffer-substring (line-beginning-position)
                             (line-end-position))))
      (paredit-comment-dwim)
    (save-excursion
      (end-of-line)
      (set-mark (line-beginning-position))
      (paredit-comment-dwim))))
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-;") 'sd/comment-dwim)))




;; basic duplicate line functionality
(defun sd/duplicate-line ()
  (interactive)
  (when (not (region-active-p))
    (beginning-of-line)
    (set-mark (line-beginning-position 2)))
  (insert
   (buffer-substring (caar (region-bounds))
                     (cdar (region-bounds)))))
(global-set-key (kbd "s-d") 'sd/duplicate-line)




;; man page color
(progn
  (require 'man)
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))




;; markdown
(progn
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))




;; misc
(global-hl-todo-mode)




;; shell scripts
(setq sh-basic-offset 2)




;; javascript
(progn
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (add-hook 'js-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'js-comint-start-or-switch-to-repl)
              (local-set-key (kbd "C-x C-e") 'js-comint-send-last-sexp)
              (local-set-key (kbd "C-x C-r") 'js-comint-send-region)
              (local-set-key (kbd "C-c C-b") 'js-comint-send-buffer)))

  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (when (string-match "\\(t\\|j\\)sx?" (file-name-extension buffer-file-name))
  ;;               (local-set-key (kbd "C-c C-e") 'ts-comint-start-or-switch-to-repl)
  ;;               (local-set-key (kbd "C-x C-e") 'ts-comint-send-last-sexp)
  ;;               (local-set-key (kbd "C-x C-r") 'ts-comint-send-region)
  ;;               (local-set-key (kbd "C-c C-b") 'ts-comint-send-buffer))))

  (defun sd/setup-tide-mode ()
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (add-hook 'before-save-hook 'tide-format-before-save))

  (require 'tide)
  (define-transient-command sd/tide-commands ()
    "TypeScript Commands"
    [["Refactoring"
      ("r" "Refactor" tide-refactor)
      ("s" "Rename symbol" tide-rename-symbol)
      ("m" "Rename file" tide-rename-file)
      ("o" "Organize imports" tide-organize-imports)]
     ["Navigating"
      ("f" "References" tide-references)
      ("i" "Jump to implementation" tide-jump-to-implementation)
      ("d" "Jump to definition" tide-jump-to-definition)
      ("l" "List servers" tide-list-servers)]
     ["Errors"
      ("x" "Fix error at point" tide-fix)
      ("n" "Add tslint-disable-next-line" tide-add-tslint-disable-next-line)
      ("e" "List project errors" tide-project-errors)]])
  (define-key tide-mode-map (kbd "s-R") 'sd/tide-commands)

  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(t\\|j\\)sx?\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-match "\\(t\\|j\\)sx?" (file-name-extension buffer-file-name))
                (sd/setup-tide-mode))))

  (require 'flycheck)
  (flycheck-add-mode 'typescript-tslint 'web-mode))




;; eshell
(progn
  (require 'eshell)

  (setq eshell-banner-message "ready.\n")

  (defun sd/open-new-eshell-at-project-root ()
    (interactive)
    (let* ((default-directory default-directory))
      (eshell 'ignored-value)))
  (global-set-key (kbd "s-t") 'sd/open-new-eshell-at-project-root)

  (defun sd/clear-eshell-buffer ()
    (interactive)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (eshell-commands (eshell-print "buffer cleared.\n"))
      (eshell-emit-prompt)))

  (defun sd/setup-eshell ()
    (setenv "TERM" "xterm-256color")
    (setq-local truncate-lines nil)
    ;; (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
    (define-key eshell-mode-map (kbd "s-k")   'sd/clear-eshell-buffer)
    (define-key eshell-mode-map (kbd "M-r")   'counsel-esh-history))
  (add-hook 'eshell-mode-hook #'sd/setup-eshell)

  (defun sd/eshell-prompt ()
    (concat
     "\n"
     (abbreviate-file-name
      (eshell/pwd))
     (if (= (user-uid) 0) " # " " $ ")))
  (setq eshell-prompt-function 'sd/eshell-prompt))




;; shortcuts for opening other apps
(progn
  (defun sd/open-gitx-here ()
    (interactive)
    (shell-command "open -agitx ."))
  (global-set-key (kbd "s-C-g") 'sd/open-gitx-here)

  (defun sd/open-terminal-here ()
    (interactive)
    (shell-command "open -aterminal ."))
  (global-set-key (kbd "s-T")   'sd/open-terminal-here)

  (defun sd/edit-emacs-init ()
    (interactive)
    (find-file user-init-file)))




;; lua
(defun sd/run-love2d ()
  (interactive)
  (make-comint-in-buffer "love2d"
                         "*love2d*"
                         "love"
                         nil
                         ".")
  (switch-to-buffer-other-window "*love2d*")
  (setq-local comint-input-sender 'sd/lua-comint-input-send))

(defun sd/send-to-love2d ()
  (interactive)
  (comint-send-string
   (get-process "love2d")
   (concat
    (buffer-substring
     (region-beginning)
     (region-end))
    (make-string 1 ?\0)
    "\n"))
  (message "ok sent"))

(defun sd/lua-comint-input-send (proc string)
  (comint-simple-send proc (concat string (make-string 1 ?\0))))

(setq lua-indent-level 2)
(add-hook 'lua-mode-hook
          (lambda ()
            (define-key lua-mode-map (kbd "s-r") 'sd/run-love2d)
            (define-key lua-mode-map (kbd "s-e") 'sd/send-to-love2d)))




;; experimental "command center"
(progn
  (require 'transient)
  (define-transient-command sd/core-commands ()
    "Emacs Commands"
    [["Core"
      ("t" "Use theme" counsel-load-theme)
      ("p" "Install package" counsel-package)
      ("k" "Key bindings" counsel-descbinds)
      ("b" "Bookmarks" counsel-bookmark)
      ("i" "Edit emacs/init.el" sd/edit-emacs-init)]
     ["Git"
      ("f" "Find file in git repo" counsel-git)
      ("g" "Find string with git-grep" counsel-git-grep)
      ("x" "Open GitX here" sd/open-gitx-here)]
     ["Shells"
      ("s" "Open Terminal.app (shell) here" sd/open-terminal-here)
      ("e" "Open eshell at project root" sd/open-new-eshell-at-project-root)]])
  (global-set-key (kbd "s-P") 'sd/core-commands))




;; start off in reasonable directory
(dired (concat (getenv "HOME") "/projects"))
