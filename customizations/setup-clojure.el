;; from https://gist.github.com/zilti/6083155

;; clj-refactor: Use C-c C-m. See https://github.com/magnars/clj-refactor.el
(defun clj-refactor-hooks ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(setq cider-annotate-completion-candidates t)
(setq cider-stacktrace-suppressed-errors nil)
