;; monkey patch this function so that it saves the buffer after it performs
;; the replacement

(defun tags-query-replace (from to &optional delimited file-list-form)
  "Do `query-replace-regexp' of FROM with TO on all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].
Fourth arg FILE-LIST-FORM non-nil means initialize the replacement loop.
Fifth and sixth arguments START and END are accepted, for compatibility
with `query-replace-regexp', and ignored.

If FILE-LIST-FORM is non-nil, it is a form to evaluate to
produce the list of files to search.

See also the documentation of the variable `tags-file-name'."
  (interactive (query-replace-read-args "Tags query replace (regexp)" t t))
  (setq tags-loop-scan `(let ,(unless (equal from (downcase from))
                                '((case-fold-search nil)))
                          (if (re-search-forward ',from nil t)
                              ;; When we find a match, move back
                              ;; to the beginning of it so perform-replace
                              ;; will see it.
                              (goto-char (match-beginning 0))))
        tags-loop-operate `(progn (perform-replace ',from ',to t t ',delimited
                                                  nil multi-query-replace-map)
                                  (save-buffer)))
  (tags-loop-continue (or file-list-form t)))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(require 'helm-projectile)
(helm-projectile-on)
