(require 'eglot)
(define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
(define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)

;; this didn't work; cryptic error message
;; (add-to-list 'eglot-server-programs '(clojure-mode . ("/Users/paulalvarez/bin/clojure-lsp")))
