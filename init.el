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


;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; edit html tags like sexps
    tagedit

    ;;js2
    js2-mode

    ;;html
    web-mode

    ;; ruby
    ;; modes
    enh-ruby-mode
    rspec-mode
    projectile-rails

    ;; linting
    rubocop

    bundler

    ;; json
    json-mode

    ;; markdown
    markdown-mode

    ;; helm
    helm
    helm-ag
    helm-rg
    helm-projectile

    ;; go
    go-mode
    go-errcheck
    flymake-go

    ;; yaml-mode
    yaml-mode

    ;; completion
    yasnippet

    ;; dockerfile
    dockerfile-mode

    ;; haskell
    haskell-mode

    ;; elixir
    elixir-mode
    alchemist

    ;; groovy
    groovy-mode

    ace-window
    avy
    company
    highlight-indentation
    magit
    zoom-window

    merlin
    tuareg

    eglot))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
   :if (eq system-type 'darwin)
   :demand t
   :config
   (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; These customizationps make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
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
 '(package-selected-packages
   (quote
    (use-package rbenv lsp-mode helm-go-package terraform-mode eglot utop merlin tuareg clojure-mode magit nhexl-mode avy alchemist elixir-mode highlight-indentation haskell-mode zoom-window tomatinho bundler ac-js2 helm-tramp ycmd yasnippet yaml-mode web-mode tagedit smex rubocop rspec-mode projectile-rails parinfer paredit markdown-mode json-mode helm-projectile helm-ag groovy-mode go-mode go-errcheck flymake-go exec-path-from-shell enh-ruby-mode dockerfile-mode company color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider auto-indent-mode ace-window)))
 '(zoom-window-mode-line-color "DarkGreen"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
