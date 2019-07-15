(setq-default indent-tabs-mode nil)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(electric-pair-mode 1)

;; (push '(?\< . ?\>) electric-pair-pairs) ; Automatically pair angle brackets

;; i'd really like for electric pair to highlight angle brackets...
;; (modify-syntax-entry ?< "(>")
;; (modify-syntax-entry ?> ")<")
