(setq-default indent-tabs-mode nil)

;; given a YAML-formatted block of IPs such as
;;
;;    looker:
;;      - 52.1.5.228/32
;;      - 52.1.157.156/32
;;      - 54.208.10.167/32
;;      - 54.209.116.191/32
;;      - 54.83.113.5/32
;;
;; the point must be somewhere before the `:`, because the first thing
;; this macro does is `C-s :`
;;
;; the result should be:
;;
;;    looker:
;;      - 52.1.5.228/32
;;      - 52.1.157.156/32
;;      - 54.83.113.5/32
;;      - 54.208.10.167/32
;;      - 54.209.116.191/32
;;

(fset 'sort-ip-blocks
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 58 14 1 6 6 24 32 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 134217829 1 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 21 134217852 25 return 67108896 18 58 134217848 115 100 backspace backspace 100 101 108 101 116 101 45 116 114 97 105 108 105 110 103 45 119 105 116 backspace backspace 104 105 116 101 115 112 97 99 101 return 134217829] 0 "%d")) arg)))

;; wip
;; (defun my-string-rectangle (start end string)
;;   "cleans up whitespae after running rectangle-string"
;;   (interactive "P"))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(electric-pair-mode 1)

;; (push '(?\< . ?\>) electric-pair-pairs) ; Automatically pair angle brackets

;; i'd really like for electric pair to highlight angle brackets...
;; (modify-syntax-entry ?< "(>")
;; (modify-syntax-entry ?> ")<")
