;; from https://gist.github.com/zilti/6083155

;; clj-refactor: Use C-c C-m. See https://github.com/magnars/clj-refactor.el
(defun clj-refactor-hooks ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(setq cider-annotate-completion-candidates t)
(setq cider-stacktrace-suppressed-errors nil)

;; TODO: get this to work?
;; (add-hook 'clojure-mode-hook 'eglot-ensure)
;; this didn't work; cryptic error message
;; (add-to-list 'eglot-server-programs '(clojure-mode . ("/Users/paulalvarez/bin/clojure-lsp")))
