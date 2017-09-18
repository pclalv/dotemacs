(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

; https://magit.vc/manual/magit/Performance.html#Performance
;; i don't care about tags
(remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
