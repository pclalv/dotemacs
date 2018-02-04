;; from https://gist.github.com/zilti/6083155

;; clj-refactor: Use C-c C-m. See https://github.com/magnars/clj-refactor.el
(defun clj-refactor-hooks ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; nREPL: The new Clojure network REPL. See https://github.com/kingtim/nrepl.el
;; (require 'nrepl)
;; (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; Autocompletion for nREPL; From https://github.com/purcell/ac-nrepl
;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes'nrepl-mode))
;; Probably gets in the way of yasnippet?
;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; Popup documentation change
;; (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; Glue code: Bring it all together...
(add-hook 'clojure-mode-hook 'parinfer-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
;; (add-hook 'clojure-mode-hook 'clj-refactor-hooks)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))


(setq cider-annotate-completion-candidates t)
(setq cider-stacktrace-suppressed-errors nil)

