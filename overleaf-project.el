;;; overleaf-project.el --- Clone, push, and pull full Overleaf projects with Git -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2026 Jamie Cui

;; Author: Jamie Cui
;; Created: April 14, 2026
;; URL: https://github.com/Jamie-Cui/overleaf-project
;; Package-Requires: ((emacs "29.4") (websocket "1.15") (webdriver "0.1"))
;; Version: 2.0.0
;; Keywords: hypermedia, tex, tools
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides project-level Overleaf integration:
;;
;; - clone a full Overleaf project to a local Git repository
;; - push committed local changes to Overleaf and pull remote updates back
;; - detect remote divergence and resolve it with normal Git merges
;;
;; Conflict resolution intentionally happens in Git, not ediff.  When
;; both local and remote changed, `overleaf-project-pull' merges the
;; downloaded remote snapshot into the current branch and leaves
;; conflicts to Magit or plain Git.

;;; Code:

(require 'overleaf-project-commands)

(provide 'overleaf-project)

;;; overleaf-project.el ends here
