(setq org-use-speed-commands t)

(setq org-startup-trunfaced nil)

;; stuff taken from
;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html

;; unic

;; /italics/ without the /s
(setq org-hide-emphasis-markers t)

;; unicode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
