(use-package ace-window
  :bind ("M-p" . ace-window))

(use-package avy
  :bind
  (("C-'" . avy-goto-char)
   ("C-:" . avy-goto-char-2)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)))

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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(use-package projectile
  ;; :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-vc)
  (add-to-list 'projectile-globally-ignored-directories "log")
  (add-to-list 'projectile-globally-ignored-directories "tmp")
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  (helm-mode 1)
  (global-unset-key (kbd "C-x c"))
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ;; rebind tab to run persistent action
   ("<tab>" . helm-execute-persistent-action)
   ;; make TAB works in terminal
   ("C-i" . helm-execute-persistent-action)
   ;; list actions using C-z
   ("C-z" . helm-select-action)))

(use-package helm-projectile
  :requires (helm projectile)
  :config
  (helm-projectile-on)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-switch-project-action 'projectile-vc)
  :bind ("C-c p s t" . helm-ag-this-file))

(use-package helm-ag
  :requires helm
  :config
  (add-to-list 'grep-find-ignored-directories "log")
  (add-to-list 'grep-find-ignored-directories "tmp")
  (add-to-list 'grep-find-ignored-directories "vendor")
  (add-to-list 'grep-find-ignored-directories "coverage"))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.

(use-package zoom-window
  :config
  (custom-set-variables '(zoom-window-mode-line-color "DarkGreen"))
  :bind ("C-x C-z" . zoom-window-zoom))

;;; fix issue where tramp hangs indefinitely
;; https://github.com/bbatsov/prelude/issues/594#issuecomment-220951394
;; (add-hook 'text-mode-hook 'projectile-mode)
;; (add-hook 'prog-mode-hook 'projectile-mode)
