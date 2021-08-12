;; ensure emacs is enforcing TLS
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html#fnref:4
(setq tls-checktrust t)

;; disable things.
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel
(global-unset-key (kbd "C-z")) ; suspend-frame (minimize/maximize frame)
(global-unset-key (kbd "C-<tab>"))
(global-unset-key (kbd "C-x f"))
;; no tabs
(setq-default indent-tabs-mode nil)

(rassq-delete-all 'dsssl-mode auto-mode-alist)
(setq inhibit-startup-screen t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1)) ; Don't show native toolbar.
(menu-bar-mode -1) ; Turn off the menu bar at the top of each frame
                   ; because it's distracting
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1)) ; Don't show native OS scroll bars for buffers
                        ; because they're redundant
(setq ring-bell-function 'ignore) ; no bell

;; utf-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; keybindings
(global-set-key (kbd "C-c P l") '(load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

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
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))
;; source: https://www.emacswiki.org/emacs/BackupDirectory#toc2
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.saves/")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ; use versioned backups

(global-auto-revert-mode t)

;; Changing behavior of C-a
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; improvements to builtins

(defadvice split-window-right
  (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-right)

(defadvice delete-window
  (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'delete-window)

(defadvice split-window-below
  (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-below)

(defun my-find-file-hook ()
  "Buffer local settings for buffers that are actually files."
  (setq indicate-empty-lines t
        show-trailing-whitespace t))
(add-hook 'find-file-hooks 'my-find-file-hook)

;; maximize window on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

(global-linum-mode)

(set-face-attribute 'linum nil :height 100)

;; don't make me type "yes" or "no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; packages

;; boilerplate from https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window))

(use-package avy
  :straight t
  :bind
  (("C-'" . avy-goto-char)
   ("C-:" . avy-goto-char-2)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)))

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :demand t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package eglot
  :straight t
  :after projectile
  :bind (:map eglot-mode-map
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format)
              ("C-c e h" . eglot-help-at-point)
              ("C-c e r" . eglot-rename))
  :config
  ;; Bridge projectile and project together so packages that depend on
  ;; project like eglot work
  ;; https://github.com/joaotavora/eglot/issues/129#issuecomment-444130367
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (add-to-list 'eglot-server-programs '(clojure-mode . ("/Users/paulalvarez/bin/clojure-lsp")))
  (add-to-list 'eglot-server-programs '(go-mode . ("/Users/paulalvarez/code/go/bin/gopls")))
  :hook
  ;; clojure-lsp still doesn't work out of the box
  ;; (clojure-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (ruby-mode . eglot-ensure))

(use-package electric
  :hook
  (ruby-mode . electric-pair-mode)
  (c-mode . electric-pair-mode)
  (cider-repl-mode . electric-pair-mode))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :straight t
  :if (eq system-type 'darwin)
  :demand t)

(use-package hideshow
  :after (ruby-mode)
  :bind
  ;; hideshow bindings suck, but this doesn't work
  (("C-c h" . hs-hide-block)
   ("C-c s" . hs-show-block))
  :config
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                       ; Block end
                 ,(rx (or "#" "=begin"))                        ; Comment start
                 ruby-forward-sexp nil))
  :hook (ruby-mode . (lambda () (hs-minor-mode))))

(use-package highlight-indentation
  :straight t
  :hook (ruby-mode . highlight-indentation-mode))

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :config
  (defun magit-open (&optional args)
    "Open the current file in github."
    (interactive (list (magit-commit-arguments)))
    (magit-run-git "open" (magit-file-relative-name)))

  :custom
  (magit-log-section-arguments '("--decorate" "-n256")))

(use-package forge
  :straight t
  :after magit)

(use-package parinfer-rust-mode
  :straight t
  :init
  (setq parinfer-rust-auto-download t)
  :hook
  (emacs-list-mode . parinfer-rust-mode)
  (emacs-lisp-mode . parinfer-rust-mode)
  (common-lisp-mode . parinfer-rust-mode)
  (scheme-mode . parinfer-rust-mode)
  (lisp-mode . parinfer-rust-mode)
  (clojure-mode . parinfer-rust-mode)
  (clojurescript-mode . parinfer-rust-mode))

;; just for package-list-packages
(use-package package
  :straight t
  :config
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

(use-package web-mode
  :straight t
  :mode ("\\.liquid\\'")
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4))

(use-package zoom-window
  :straight t
  :config
  (setq zoom-window-mode-line-color "DarkGreen")
  :bind ("C-x C-z" . zoom-window-zoom))

;; languages

;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

(use-package clojure-mode
  :straight t)

(use-package clojure-mode-extra-font-locking
  :straight t)

(use-package cider
  :straight t
  :config
  (setq cider-annotate-completion-candidates t)
  (setq cider-stacktrace-suppressed-errors nil)
  :custom
  (cider-repl-history-file "~/.cider-history.eld"))

(use-package dockerfile-mode
  :straight t)

;;;;;;;;;;;;
;; elixir ;;
;;;;;;;;;;;;

(use-package elixir-mode
  :straight t)

(use-package alchemist
  :straight t)

;;;;;;;;;;;;
;; golang ;;
;;;;;;;;;;;;

(use-package go-mode
  :straight t
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
    (if (not (string-match "go" compile-command))   ; set compile command default
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package go-errcheck
  :straight t)

(use-package flymake-go
  :straight t)

;;;;;;;;;;;;
;; groovy ;;
;;;;;;;;;;;;

(use-package groovy-mode
  :straight t
  :mode ("\\.dsl\\'"))

(use-package haskell-mode
  :straight t)

(use-package json-mode
  :straight t
  :mode ("\\.json\\'"))

(use-package markdown-mode
  :straight t
  :mode ("\\.markdown\\'" "\\.md\\'"))

;;;;;;;;;;
;; ruby ;;
;;;;;;;;;;

(use-package ruby-mode
  :straight t
  :config
  (setq ruby-use-smie nil)
  :custom
  (ruby-deep-indent-paren nil)
  (ruby-indent-level 4))

(use-package inf-ruby
  :straight t
  :custom
  (inf-ruby-default-implementation "pry"))

(use-package rspec-mode
  :straight t)

(use-package terraform-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

(use-package sh-script
  ;; Configuration of the built-in sh-mode
  :mode ("\\.bats\\'" . shell-script-mode)
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))
;; who knows when bash-language-server will work
;; (add-hook 'sh-mode-hook 'eglot-ensure))

(use-package z80-mode
  :load-path "vendor/")

(use-package jsonnet-mode
  :straight t)

(add-to-list 'load-path "~/.emacs.d/vendor")

(add-to-list 'load-path "~/.emacs.d/customizations")
(load "navigation.el")
(load "misc.el")
(load "setup-ocaml.el")
(load "setup-org.el")
(load "setup-smalltalk.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/Dropbox (Personal)/habits.org"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
