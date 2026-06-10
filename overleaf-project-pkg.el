;;; overleaf-project-pkg.el --- Package metadata for overleaf-project  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Version: 2.0.0
;; Package-Requires: ((emacs "29.4") (websocket "1.15") (webdriver "0.1"))
;; Keywords: hypermedia, tex, tools
;; URL: https://github.com/Jamie-Cui/overleaf-project

;;; Commentary:

;; Package metadata for overleaf-project.

;;; Code:

(define-package "overleaf-project" "2.0.0"
  "Clone, push, and pull full Overleaf projects with Git"
  '((emacs "29.4")
    (websocket "1.15")
    (webdriver "0.1"))
  :url "https://github.com/Jamie-Cui/overleaf-project"
  :keywords '("hypermedia" "tex" "tools")
  :authors '(("Jamie Cui" . "jamie.cui@outlook.com"))
  :maintainers '(("Jamie Cui" . "jamie.cui@outlook.com")))

;;; overleaf-project-pkg.el ends here
