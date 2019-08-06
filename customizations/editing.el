(setq-default indent-tabs-mode nil
              tab-width 4)

(defun my-find-file-hook ()
  "Buffer local settings for buffers that are actually files."
  (setq indicate-empty-lines t
        show-trailing-whitespace t))
(add-hook 'find-file-hooks 'my-find-file-hook)

;; (push '(?\< . ?\>) electric-pair-pairs) ; Automatically pair angle brackets

;; i'd really like for electric pair to highlight angle brackets...
;; (modify-syntax-entry ?< "(>")
;; (modify-syntax-entry ?> ")<")

;; don't make me type "yes" or "no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; Don't show native toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; no bell
(setq ring-bell-function 'ignore)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; maximize window on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Rebalance windows after splitting right (i.e. C-x-3)
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

(set-face-attribute 'linum nil :height 100)

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.10.0/")
  (eval-after-load 'rspec-mode
   '(rspec-install-snippets)))
