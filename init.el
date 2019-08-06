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

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :demand t)

(use-package eglot
  :straight t
  :after projectile
  :bind (:map eglot-mode-map
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format)
              ("C-c e h" . eglot-help-at-point)
              ("C-c e r" . eglot-rename)
              ("C-c h" . eglot-help-at-point))
  :config
  ;; Bridge projectile and project together so packages that depend on
  ;; project like eglot work
  ;; https://github.com/joaotavora/eglot/issues/129#issuecomment-444130367
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (add-to-list 'eglot-server-programs '(go-mode . ("/Users/paulalvarez/code/go/bin/gopls")))
  :hook
  (ruby-mode . eglot-ensure)
  (go-mode . eglot-ensure))

(use-package electric
  :hook (ruby-mode . electric-pair-mode))

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

(use-package paredit
  :straight t)

(use-package yasnippet
  :straight t)

;; languages

;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

(use-package clojure-mode
  :straight t)

(use-package clojure-mode-extra-font-locking
  :straight t)

(use-package cider
  :straight t)

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
  :straight t)

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
  :mode ("\\.markdown\\'" "\\.md\\'"))

;;;;;;;;;;
;; ruby ;;
;;;;;;;;;;

(use-package ruby-mode
  :straight t)

(use-package inf-ruby
  :straight t)

(use-package rspec-mode
  :straight t)

(use-package terraform-mode
  :straight t)

(use-package yaml-mode
  :straight
  :mode ("\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

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
(load "setup-lisps.el")
(load "setup-ocaml.el")
(load "setup-org.el")
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
