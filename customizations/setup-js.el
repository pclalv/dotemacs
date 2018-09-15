(add-hook 'js2-mode-hook 'highlight-indentation-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js-indent-level 2)

(setq js2-highlight-level 3)

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'js-mode-hook 'my-paredit-nonlisp) ;use with the above function

(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
