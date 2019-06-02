(require 'eglot)
(define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
(define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)

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
