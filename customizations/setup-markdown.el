(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; wip; don't count 1. 2. 3. instead count 1. 1. 1.
;; (defun markdown-insert-list-item (&optional arg)
;;   "Insert a new list item.
;; If the point is inside unordered list, insert a bullet mark.  If
;; the point is inside ordered list, insert the next number followed
;; by a period.  Use the previous list item to determine the amount
;; of whitespace to place before and after list markers.

;; With a \\[universal-argument] prefix (i.e., when ARG is (4)),
;; decrease the indentation by one level.

;; With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
;; increase the indentation by one level."
;;   (interactive "p")
;;   (let (bounds cur-indent marker indent new-indent new-loc)
;;     (save-match-data
;;       ;; Look for a list item on current or previous non-blank line
;;       (save-excursion
;;         (while (and (not (setq bounds (markdown-cur-list-item-bounds)))
;;                     (not (bobp))
;;                     (markdown-cur-line-blank-p))
;;           (forward-line -1)))
;;       (when bounds
;;         (cond ((save-excursion
;;                  (skip-chars-backward " \t")
;;                  (looking-at markdown-regex-list))
;;                (beginning-of-line)
;;                (insert "\n")
;;                (forward-line -1))
;;               ((not (markdown-cur-line-blank-p))
;;                (newline)))
;;         (setq new-loc (point)))
;;       ;; Look ahead for a list item on next non-blank line
;;       (unless bounds
;;         (save-excursion
;;           (while (and (null bounds)
;;                       (not (eobp))
;;                       (markdown-cur-line-blank-p))
;;             (forward-line)
;;             (setq bounds (markdown-cur-list-item-bounds))))
;;         (when bounds
;;           (setq new-loc (point))
;;           (unless (markdown-cur-line-blank-p)
;;             (newline))))
;;       (if (not bounds)
;;           ;; When not in a list, start a new unordered one
;;           (progn
;;             (unless (markdown-cur-line-blank-p)
;;               (insert "\n"))
;;             (insert markdown-unordered-list-item-prefix))
;;         ;; Compute indentation and marker for new list item
;;         (setq cur-indent (nth 2 bounds))
;;         (setq marker (nth 4 bounds))
;;         (cond
;;          ;; Dedent: decrement indentation, find previous marker.
;;          ((= arg 4)
;;           (setq indent (max (- cur-indent 4) 0))
;;           (let ((prev-bounds
;;                  (save-excursion
;;                    (when (markdown-prev-list-item (- (nth 3 bounds) 1))
;;                      (markdown-cur-list-item-bounds)))))
;;             (when prev-bounds
;;               (setq marker (nth 4 prev-bounds)))))
;;          ;; Indent: increment indentation by 4, use same marker.
;;          ((= arg 16) (setq indent (+ cur-indent 4)))
;;          ;; Same level: keep current indentation and marker.
;;          (t (setq indent cur-indent)))
;;         (setq new-indent (make-string indent 32))
;;         (goto-char new-loc)
;;         (cond
;;          ;; Ordered list
;;          ((string-match "[0-9]" marker)
;;           (if (= arg 16) ;; starting a new column indented one more level
;;               (insert (concat new-indent "1. "))
;;             ;; travel up to the last item and pick the correct number.  If
;;             ;; the argument was nil, "new-indent = cur-indent" is the same,
;;             ;; so we don't need special treatment. Neat.
;;             (save-excursion
;;               (while (and (not (looking-at (concat new-indent "\\([0-9]+\\)\\(\\.[ \t]*\\)")))
;;                           (>= (forward-line -1) 0))))
;;             (let* ((old-prefix (match-string 1))
;;                    (old-spacing (match-string 2))
;;                    (new-prefix (if markdown-ordered-list-no-increment
;;                                    "1"
;;                                    (if old-prefix
;;                                        (int-to-string (1+ (string-to-number old-prefix)))
;;                                      "1")))
;;                    (space-adjust (- (length old-prefix) (length new-prefix)))
;;                    (new-spacing (if (and (match-string 2)
;;                                          (not (string-match "\t" old-spacing))
;;                                          (< space-adjust 0)
;;                                          (> space-adjust (- 1 (length (match-string 2)))))
;;                                     (substring (match-string 2) 0 space-adjust)
;;                                   (or old-spacing ". "))))
;;               (insert (concat new-indent new-prefix new-spacing)))))
;;          ;; Unordered list
;;          ((string-match "[\\*\\+-]" marker)
;;           (insert new-indent marker)))))))
