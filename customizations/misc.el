(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "C-c P l") '(load-file "~/.emacs.d/init.el"))

;; Disable commonly unintended key presses.
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel

(setq backup-directory-alist
      '(("." . "/tmp/emacs-backups")))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(global-auto-revert-mode t)

;; default to utf-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
