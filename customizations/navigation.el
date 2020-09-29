(use-package projectile
  :straight t
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-vc)
  (add-to-list 'projectile-globally-ignored-directories "log")
  (add-to-list 'projectile-globally-ignored-directories "tmp")
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

;; consider using selectrum and prescient instead, now that helm has been ceased development
(use-package helm
  :straight t
  :demand t
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  (helm-mode 1)
  ;; The default `helm-command-prefix' "C-x c" is quite close to "C-x
  ;; C-c", which quits Emacs.
  :bind
  (("M-x" . helm-M-x)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ;; rebind tab to run persistent action
   ("C-i" . helm-execute-persistent-action) ;; make TAB works in terminal
   ("C-z" . helm-select-action))) ;; list actions using C-z
   

(use-package helm-projectile
  :straight t
  :demand t
  :requires (helm projectile)
  :bind (:map helm-command-map ("s" . helm-ag-this-file))
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

(use-package helm-rg
  :straight t
  :requires helm)

(use-package helm-ag
  :straight t
  :requires helm
  :config
  (add-to-list 'grep-find-ignored-directories "log")
  (add-to-list 'grep-find-ignored-directories "tmp")
  (add-to-list 'grep-find-ignored-directories "vendor")
  (add-to-list 'grep-find-ignored-directories "coverage")
  (add-to-list 'grep-find-ignored-directories "_build")
  (add-to-list 'grep-find-ignored-directories "node_modules"))
