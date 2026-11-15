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

(defgroup magit-git-graph nil
  "Show git-graph git log layout in a Magit buffer."
  :group 'magit-log)

(defvar-keymap magit-git-graph-mode-map
  :doc "Keymap for `magit-git-graph-mode'."
  :parent magit-log-mode-map)

(define-derived-mode magit-git-graph-mode magit-log-mode "Magit Git Graph"
  "Mode for looking at Git log through git-graph executable.

Each commit is a regular magit commit section, so commands that act on
the commit at point work just like they would in `magit-log-mode'.

\\{magit-git-graph-mode-map}"
  :interactive nil
  :group 'magit-git-graph)
