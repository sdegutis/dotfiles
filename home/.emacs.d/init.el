(setq sd/packages
      '(

        ;; themes
        spacemacs-theme
        zenburn-theme
        color-theme-sanityinc-tomorrow

        ;; editing helpers
        undo-tree
        multiple-cursors
        rainbow-delimiters
        paredit

        dimmer

        ;; buffer management
        buffer-move
        windsize

        ;; git
        magit

        ;; eshell
        xterm-color

        ;; fuzzy-completion
        counsel
        flx
        smex
        ivy
        ido-vertical-mode
        company
        company-flx

        ))

;; setup initial packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p (car sd/packages))
  (package-refresh-contents)
  (mapc #'package-install sd/packages))

;; set theme
(progn
  (defun sd/use-theme (name)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme name t t)
    (enable-theme name))

  ;; (sd/use-theme 'spacemacs-light)
  (sd/use-theme 'spacemacs-dark)
  ;; (sd/use-theme 'zenburn)
  ;; (sd/use-theme 'sanityinc-tomorrow-eighties)
  ;; (sd/use-theme 'sanityinc-tomorrow-night)
  ;; (sd/use-theme 'monokai)
  ;; (sd/use-theme 'naquadah)
  )

(defun sd/try-theme ()
  (interactive)
  (sd/use-theme
   (intern
    (completing-read
     "Theme> "
     '(spacemacs-light
       spacemacs-dark
       zenburn
       sanityinc-tomorrow-eighties
       sanityinc-tomorrow-night
       monokai
       naquadah)))))

(global-unset-key (kbd "s-k"))
(global-set-key (kbd "s-k s-t") #'sd/try-theme)



;; fundamental fixes to emacs's stupid defaults
(progn
  ;; "customize"
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; (load custom-file)

  ;; startup stuff
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq initial-scratch-message "")
  (setq inhibit-startup-screen t)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-echo-area-message (user-real-login-name))

  ;; better defaults
  (setq enable-recursive-minibuffers t)
  (setq default-frame-alist '((vertical-scroll-bars)))
  (set-display-table-slot standard-display-table 0 ?~)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq require-final-newline t)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (global-hl-line-mode 1)
  (delete-selection-mode 1)
  (setq-default cursor-type 'box)
  (fset 'yes-or-no-p 'y-or-n-p)
  (blink-cursor-mode 1)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq ring-bell-function 'ignore)
  (setq blink-matching-paren t)
  (pixel-scroll-mode 0) ;; somehow they managed to make a pixel scroll
                        ;; that's actually worse than not having it

  (setq scroll-conservatively 101)
  (setq auto-window-vscroll nil)
  (setq scroll-step 1)

  ;; useless shortcuts
  (global-unset-key (kbd "C-z")) ;; stop minimizing
  (global-unset-key (kbd "s-p")) ;; stop asking to print

  ;; visual tweaks
  (add-to-list 'default-frame-alist '(alpha 100 100))
  (set-face-font 'default "Menlo-14.0")

  ;; dark mode stuff?
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; disable backup, auto-save, and lock files
  (setq-default backup-inhibited t)
  (setq-default auto-save-default nil)
  (setq create-lockfiles nil)

  ;; prefer to split windows side by side
  (setq split-height-threshold nil)
  (setq split-width-threshold 125)

  ;; dont wrap lines when editing plain text
  (add-hook 'text-mode-hook 'toggle-word-wrap)
  (add-hook 'text-mode-hook 'toggle-truncate-lines)

  ;; join-line
  (defun sd/join-line () (interactive) (join-line 1))
  (global-set-key (kbd "M-k") #'sd/join-line)

  ;; fix $PATH (here for dired *and* eshell)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

  ;; editing
  (define-key prog-mode-map (kbd "RET") 'newline-and-indent))

;; buffer management
(progn
  (dimmer-mode 1)

  ;; buffer/panel shortcuts
  (global-set-key (kbd "s-[") 'previous-buffer)
  (global-set-key (kbd "s-]") 'next-buffer)
  (global-set-key (kbd "s-{") '(lambda () (interactive) (other-window -1)))
  (global-set-key (kbd "s-}") 'other-window)
  (global-set-key (kbd "<C-s-up>")     'buf-move-up)
  (global-set-key (kbd "<C-s-down>")   'buf-move-down)
  (global-set-key (kbd "<C-s-left>")   'buf-move-left)
  (global-set-key (kbd "<C-s-right>")  'buf-move-right)
  (require 'windsize)
  (windsize-default-keybindings)

  ;; new blank buffers easily
  (defun sd/new-blank-bufer ()
    (interactive)
    (switch-to-buffer (generate-new-buffer-name "untitled"))
    (text-mode))
  (global-set-key (kbd "s-N") 'sd/new-blank-bufer)

  ;; unique buffer names
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t))

;; javascript
(setq js-indent-level 2)

;; paredit
(autoload 'enable-paredit-mode "paredit" nil t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "s-z") #'undo-tree-undo)
(global-set-key (kbd "s-Z") #'undo-tree-redo)

(require 'dired-x)
(setq-default dired-auto-revert-buffer t)
(setq-default dired-use-ls-dired nil)
(setq dired-listing-switches "-alh")

(require 'magit)
(setq vc-follow-symlinks t)
(add-to-list 'magit-no-confirm 'stage-all-changes)
(add-to-list 'magit-no-confirm 'unstage-all-changes)
(global-set-key (kbd "s-G") #'magit-status)

;; region shortcuts
(global-set-key (kbd "C-c w") #'count-words-region)
(global-set-key (kbd "C-c u") #'upcase-region)
(global-set-key (kbd "C-c d") #'downcase-region)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; edit multiple lines
(require 'multiple-cursors)
(global-set-key (kbd "C-<return>") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-s->") 'mc/mark-next-like-this-symbol)

;; mode line
(setq-default
 mode-line-format
 '("%e" (:eval (list " %* " mode-line-buffer-identification
                     "  %3l  " (abbreviate-file-name default-directory)
                     " " mode-line-modes
                     " " mode-line-misc-info))))

;; vertical fuzzy-matching everywhere
(progn
  (require 'ivy)
  (require 'smex)
  (require 'flx)

  ;; stop backspace closing minibuffer
  (setq ivy-on-del-error-function nil)

  ;; ivy fuzzy matching
  (setq ivy-re-builders-alist
        '((swiper . regexp-quote)
          (t      . ivy--regex-fuzzy)))

  (global-set-key (kbd "M-x")     'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f")  'counsel-describe-function)
  (global-set-key (kbd "<f1> v")  'counsel-describe-variable)
  (global-set-key (kbd "<f1> l")  'counsel-find-library)
  (global-set-key (kbd "<f2> i")  'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u")  'counsel-unicode-char)
  (global-set-key (kbd "C-c g")   'counsel-git)
  (global-set-key (kbd "C-c j")   'counsel-git-grep)
  (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)

  (setq ivy-extra-directories nil)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  (ivy-mode 1)

  ;; ido for buffer management
  (ido-mode 1)
  (ido-vertical-mode 1)

  ;; auto-completion with fuzzy matching
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (company-flx-mode +1)))

;; eshell
(progn
  (require 'eshell)

  (setenv "TERM" "xterm-256color")

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

  (setq eshell-banner-message "ready.\n")
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq eshell-prompt-function 'sd/eshell-prompt)

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
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
    (setq exec-path (append '("/usr/local/bin") exec-path))
    (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
    (define-key eshell-mode-map (kbd "s-k")   'sd/clear-eshell-buffer)
    (define-key eshell-mode-map (kbd "M-r")   'counsel-esh-history))

  (defun sd/eshell-prompt ()
    (concat
     "\n"
     (abbreviate-file-name
      (eshell/pwd))
     (if (= (user-uid) 0) " # " " $ ")))

  (add-hook 'eshell-mode-hook 'toggle-truncate-lines)
  (add-hook 'eshell-mode-hook #'sd/setup-eshell)
  (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t))))

;; shortcuts for opening other apps
(progn
  (defun sd/open-gitx-here ()
    (interactive)
    (shell-command "open -agitx ."))
  (global-set-key (kbd "s-C-g") 'sd/open-gitx-here)

  (defun sd/open-terminal-here ()
    (interactive)
    (shell-command "open -aterminal ."))
  (global-set-key (kbd "s-T")   'sd/open-terminal-here))

;; start off in reasonable directory
(dired (concat (getenv "HOME") "/projects"))
