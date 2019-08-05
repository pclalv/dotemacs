(use-package eglot
  :bind (:map eglot-mode-map
         ("C-c e a" . eglot-code-actions)
         ("C-c e f" . eglot-format)
         ("C-c e h" . eglot-help-at-point)
         ("C-c e r" . eglot-rename)
         ("C-c h" . eglot-help-at-point)))

;; this didn't work; cryptic error message
;; (add-to-list 'eglot-server-programs '(clojure-mode . ("/Users/paulalvarez/bin/clojure-lsp")))

;; Bridge projectile and project together so packages that depend on
;; project like eglot work
;; https://github.com/joaotavora/eglot/issues/129#issuecomment-444130367
(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(projectile-mode t)

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))
