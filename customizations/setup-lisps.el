(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'common-lisp-mode-hook #'parinfer-mode)
(add-hook 'scheme-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)

;; parinfer
(add-hook 'parinfer-mode-hook
          (lambda ()
            (local-set-key (kbd "C-,") 'parinfer-toggle-mode)))
