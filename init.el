(require 'package)

;; TODO: ensure emacs is enforcing TLS
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html#fnref:4
;; $ brew uninstall emacs-mac && brew install emacs-mac --with-gnutls
;; (setq tls-checktrust t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

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

    eglot
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
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

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
;; (load "shell-integration.el")

;; These customizationps make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
;; (load "elisp-editing.el")

;; Langauage-specific
(load "setup-c.el")
(load "setup-clojure.el")
(load "setup-eglot.el")
;; (load "setup-elixir.el")
;; (load "setup-html.el")
(load "setup-js.el")
;; (load "setup-json.el")
;; (load "setup-haskell.el")
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
(load "setup-version-control.el")
(load "setup-yaml.el")
(load "setup-wanderlust.el")

(setq inhibit-startup-screen t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-log-section-arguments (quote ("--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (terraform-mode eglot utop merlin tuareg clojure-mode magit wanderlust nhexl-mode avy alchemist elixir-mode highlight-indentation haskell-mode zoom-window tomatinho bundler js2-mode ac-js2 helm-tramp jsx-mode ycmd yasnippet yaml-mode web-mode tagedit smex rubocop rspec-mode projectile-rails parinfer paredit markdown-mode json-mode helm-projectile helm-ag groovy-mode go-mode go-errcheck flymake-go exec-path-from-shell enh-ruby-mode dockerfile-mode company color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider auto-indent-mode ace-window)))
 '(zoom-window-mode-line-color "DarkGreen"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
