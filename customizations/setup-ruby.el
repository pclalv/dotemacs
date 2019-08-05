(use-package ruby-mode)

(use-package highlight-indentation
  :hook (ruby-mode . highlight-indentation-mode))

(use-package electric
  :hook (ruby-mode . electric-pair-mode))

(use-package hideshow
  :after (ruby-mode)
  :bind
  ;; hideshow bindings suck, but this doesn't work
  (("C-c h" . hs-hide-block)
   ("C-c s" . hs-show-block))
  :config
  (add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil))
  :hook (ruby-mode . (lambda () (hs-minor-mode))))
