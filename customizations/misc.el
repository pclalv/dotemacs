(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backups"))

(setq backup-directory-alist
      '(("." . "/tmp/emacs-backups")))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(global-auto-revert-mode t)

;; for mitsuhara's port
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
