(require 'tramp)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "C-c P l") '(load-file "~/.emacs.d/init.el"))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;; Disable commonly unintended key presses.
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel
(global-unset-key (kbd "C-z")) ; suspend-frame (minimize/maximize frame)

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

;; for mitsuhara's port
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
