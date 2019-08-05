(setq-default indent-tabs-mode nil
              tab-width 4)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(electric-pair-mode 1)

;; (push '(?\< . ?\>) electric-pair-pairs) ; Automatically pair angle brackets

;; i'd really like for electric pair to highlight angle brackets...
;; (modify-syntax-entry ?< "(>")
;; (modify-syntax-entry ?> ")<")

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.10.0/")
  (eval-after-load 'rspec-mode
   '(rspec-install-snippets)))
