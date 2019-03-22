;; shell scripts
(add-hook 'sh-mode-hook 'eglot-ensure)

(setq sh-basic-offset 2
      sh-indentation 2)

(add-to-list 'interpreter-mode-alist '("bats" . sh-mode))
