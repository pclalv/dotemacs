(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backups"))

;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; source: https://www.emacswiki.org/emacs/BackupDirectory#toc2
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(global-auto-revert-mode t)

;; for mitsuhara's port
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
