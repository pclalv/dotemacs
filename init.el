;; ensure emacs is enforcing TLS
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html#fnref:4
(setq tls-checktrust t)

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
(setq straight-use-package-by-default t)

(use-package clojure-mode
  :straight t)

(use-package clojure-mode-extra-font-locking
  :straight t)

(use-package cider
  :straight t)

(use-package paredit
  :straight t)

(use-package json-mode
  :straight t)

(use-package markdown-mode
  :straight t)

(use-package go-mode
  :straight t)

(use-package go-errcheck
  :straight t)

(use-package flymake-go
  :straight t)

(use-package yaml-mode
  :straight t)

;; completion
(use-package yasnippet
  :straight t)

;; dockerfile
(use-package dockerfile-mode
  :straight t)

;; haskell
(use-package haskell-mode
  :straight t)

;; elixir
(use-package elixir-mode
  :straight t)
(use-package alchemist
  :straight t)

;; groovy
(use-package groovy-mode
  :straight t
  :mode ("\\.dsl\\'"))

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

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :demand t)

(use-package terraform-mode
  :straight t)

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

(load "navigation.el")
(load "editing.el")
(load "misc.el")

;; Langauage-specific
(load "setup-c.el")
(load "setup-clojure.el")
(load "setup-dsssl-mode.el")
(load "setup-eglot.el")
(load "setup-go.el")
(load "setup-js.el")
(load "setup-groovy.el")
(load "setup-lisps.el")
(load "setup-markdown.el")
(load "setup-makefile.el")
(load "setup-magit.el")
(load "setup-ocaml.el")
(load "setup-org.el")
(load "setup-projectile.el")
(load "setup-projectile-rails.el")
(load "setup-ruby.el")
(load "setup-shell.el")
(load "setup-smalltalk.el")
(load "setup-yaml.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inf-ruby-default-implementation "pry")
 '(magit-log-section-arguments (quote ("--decorate" "-n256")))
