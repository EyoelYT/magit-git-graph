;;; magit-git-graph.el --- Magit buffer with git-graph's layout  -*- lexical-binding:t -*-

;; Author: Eyoel Tesfu <eyoelytesfu@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This package needs the git-graph executable: https://github.com/mlange-42/git-graph
;; This package needs to load after Magit: (with-eval-after-load 'magit (require 'magit-git-graph))

;;; Commentary:

;; Provides a `magit-log' like interface, but the constructs come from the
;; git-graph executable, not git.

;;; Code:

(require 'magit)
(require 'ansi-color)

;;; Variables

(defgroup magit-git-graph nil
  "Show git-graph git log layout in a Magit buffer."
  :group 'magit-log)

(defcustom magit-git-graph-executable "git-graph"
  "The git-graph executable"
  :type 'string)

(defcustom magit-git-graph-default-arguments '("-n256" "--style=round")
  "Arguments for `magit-git-graph'."
  :type '(repeat string))

(defcustom magit-git-graph-author-width 18
  "Width of the author name when it is shown inline instead of in the margin."
  :type 'integer)

(defvar-keymap magit-git-graph-mode-map
  :doc "Keymap for `magit-git-graph-mode'."
  :parent magit-log-mode-map
  "<remap> <magit-log-refresh>" #'magit-git-graph)

(define-derived-mode magit-git-graph-mode magit-log-mode "Magit Git Graph"
  "Mode for looking at Git log through git-graph executable.

Each commit is a regular magit commit section, so commands that act on
the commit at point work just like they would in `magit-log-mode'.

\\{magit-git-graph-mode-map}"
  :interactive nil
  :group 'magit-git-graph)

;;; Buffer

(defun magit-git-graph--arguments ()
  "Translate `magit-buffer-log-args' into git-graph arguments, and drop the rest."
  (mapcan (lambda (arg)
            (cond ((not (stringp arg)) nil)
                  ((string-match "\\`-n\\([0-9]+\\)\\'" arg)
                   (list "--max-count" (match-string 1 arg)))
                  ((string-match-p
                    "\\`--\\(?:\\(?:style\\|model\\)=.+\\|local\\|sparse\\)\\'" arg)
                   (list arg))))
          magit-buffer-log-args))

(defun magit-git-graph--run (args)
  "Run git-graph with ARGS.
Return a list of (HASH GRAPH . GRAPH-ONLY-LINES) as one element per commit.

E.g. each list member looks like this:
('COMMIT-HASH' 'MAIN-GRAPH-LINE' 'EXTRA-GRAPH-LINE-1' 'EXTRA-GRAPH-LINE-2' ...)

Element 1 (HASH): The 40+ character commit hash string.

Element 2 (MAIN-GRAPH-LINE): Graph characters (like * or | *) that
appear on the same line as the commit hash, with terminal ANSI color
codes converted into Emacs text properties (font-lock-face).

Elements 3+ (GRAPH-ONLY-LINES): Any connector lines (like |/ or |\\)
that appear directly below that commit before the next commit starts. If
there are none, the sublist will only have the first 2 elements."
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8)
           (exit (apply #'process-file magit-git-graph-executable nil t nil
                        "--color" "always" "--format" "%H"
                        (append args '("--wrap" "none"))))
           (commits nil))
      (if (not (eq exit 0))
          (user-error "git-graph failed: %s" (string-trim (buffer-string))))
      ;; apply ansi colors on terminal output
      (let ((ansi-color-apply-face-function
             (lambda (beg end face)
               (put-text-property beg end 'font-lock-face
                                  (or face 'magit-log-graph)))))
        (ansi-color-apply-on-region (point-min) (point-max)))
      ;; find the commits
      (dolist (line (split-string (buffer-string) "\n" t))
        (cond ((string-match "\\([0-9a-f]\\{40,\\}\\)[[:space:]]*\\'" line)
               (push (list (match-string-no-properties 1 line)
                           (substring line 0 (match-beginning 1)))
                     commits))
              ;; a line with initial space belongs to the commit above
              ((and commits (string-match-p "[^[:space:]]" line))
               (nconc (car commits) (list (string-trim-right line))))))
    (nreverse commits))))

(defun magit-git-graph--commit-info (hashes)
  "Return a table mapping HASHES to (REFS AUTHOR DATE SUBJECT), as git reports.
Use the same placeholders as `magit-log-heading-format'."
  (let ((table (make-hash-table :test #'equal)))
    (while hashes
      (dolist (line (magit-git-lines "log" "--no-walk=unsorted" "--decorate=full"
                                     "--format=%H%x0c%D%x0c%aN%x0c%at%x0c%s"
                                     (seq-take hashes 500)))
        (let ((fields (split-string line "\f")))
          ;; the git-hash is the key, the rest are the values, stored in a list
          (puthash (car fields) (cdr fields) table)))
      (setq hashes (nthcdr 500 hashes)))
    table))

(defun magit-git-graph--insert-author-date (author date)
  "Insert AUTHOR and the age of DATE right-aligned."
  (pcase-let ((`(,count ,unit) (magit--age date nil)))
    (insert ?\s
            (propertize " " 'display
                        `(space :align-to
                                (- right ,(+ magit-git-graph-author-width 12))))
            (propertize (truncate-string-to-width
                         (or author "") magit-git-graph-author-width nil ?\s "…")
                        'font-lock-face 'magit-log-author)
            (propertize (format " %2d %-7s" count unit)
                        'font-lock-face 'magit-log-date))))

(defun magit-git-graph--insert-commit (rev graph more info margin)
  "Insert a commit section for REV.
GRAPH is its graph prefix, MORE are the graph-only lines below it, INFO
is (REFS AUTHOR DATE SUBJECT), and MARGIN says if Magit's margin is on."
  (pcase-let* ((`(,refs ,author ,date ,subject) info)
               (refs (and refs (not (string-empty-p refs))
                          (magit-format-ref-labels refs))))
    ;; insert the graph, summary, author, etc.
    (magit-insert-section (commit rev)
      (insert (propertize rev 'font-lock-face 'magit-hash) ?\s graph)
      (when (and refs (not magit-log-show-refname-after-summary))
        (insert refs ?\s))
      (insert (magit-log--wash-summary (or subject "")))
      (when (and refs magit-log-show-refname-after-summary)
        (insert ?\s refs))
      (when (and date (not margin))
        (magit-git-graph--insert-author-date author date))
      (insert ?\n)
      ;; Margin calls in the same order as `magit-log-wash-rev'
      (when margin
        (magit-log-format-margin rev author date))
      (dolist (line more)
        (insert (make-string (1+ (length rev)) ?\s) line ?\n)
        (when margin
          (magit-make-margin-overlay))))))

(defun magit-git-graph-refresh-buffer ()
  (let* ((args (magit-git-graph--arguments))
         (commits (magit-git-graph--run args))
         (info (magit-git-graph--commit-info (mapcar #'car commits)))
         (abbrev (magit-abbrev-length))
         (margin (and (fboundp 'magit--right-margin-active)
                      (magit--right-margin-active))))
    (magit-set-header-line-format (string-join (cons "git-graph" args) " "))
    (magit-insert-section (graphbuf)
      (pcase-dolist (`(,hash ,graph . ,more) commits)
        (magit-git-graph--insert-commit (substring hash 0 abbrev) graph more
                                        (gethash hash info) margin))
      (when (eq (length commits) (magit-log-get-commit-limit))
        (magit-insert-section (longer)
          (insert-text-button
           (substitute-command-keys
            (concat "Type \\<magit-git-graph-mode-map>"
                    "\\[magit-log-double-commit-limit] to show more history"))
           'action (lambda (_) (magit-log-double-commit-limit))
           'follow-link t
           'mouse-face 'magit-section-highlight))))))

;;; Menu

(defun magit-git-graph--saved-arguments ()
  (if-let* ((saved (assq 'magit-git-graph transient-values)))
      (cdr saved)
    magit-git-graph-default-arguments))

(defun magit-git-graph--init-value (obj)
  "Use the graph buffer's arguments, else use the set, saved, or default ones."
  (let ((value (cond ((derived-mode-p 'magit-git-graph-mode)
                      magit-buffer-log-args)
                     ((and (slot-boundp obj 'value) (oref obj value)))
                     ((magit-git-graph--saved-arguments)))))
    (oset obj value value)
    value))

(defun magit-git-graph-show (args)
  "Show the history of all branches laid out by git-graph using ARGS."
  (interactive
   (list (cond ((eq transient-current-command 'magit-git-graph)
                (transient-args 'magit-git-graph))
               ((derived-mode-p 'magit-git-graph-mode)
                magit-buffer-log-args)
               ((magit-git-graph--saved-arguments)))))
  (with-current-buffer
      (magit-setup-buffer #'magit-git-graph-mode nil
        (magit-buffer-log-args args))
    (when (magit-log-goto-same-commit)
      (magit-section-update-highlight))))

(transient-define-prefix magit-git-graph ()
  "Show git log laid out by git-graph."
  :init-value #'magit-git-graph--init-value
  ["Arguments"
   (magit-log:-n)
   ("-l" "Only local branches" "--local")
   ("-S" "Sparse layout"       "--sparse")]
  ["Graph"
   ("g" magit-git-graph-show
    :description (lambda ()
                   (if (derived-mode-p 'magit-git-graph-mode) "refresh" "show")))])

;; Open the menu from Magit's log menu with "l g".
(transient-append-suffix 'magit-log "s"
  '("g" "git-graph" magit-git-graph))

(provide 'magit-git-graph)
;;; magit-git-graph.el ends here
