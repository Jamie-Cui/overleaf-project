# Repository Guidelines

## Project Structure & Module Organization
`overleaf-project.el` is the only package entrypoint. It contains authentication, project discovery, snapshot download/upload, websocket tree fetch, Git-backed push/pull logic, conflict-branch handling, and interactive commands. `README.org` is the user-facing manual and should change with any behavior, setup, or keybinding update. CI lives in `.github/workflows/melpazoid.yml`. There is no separate `test/` directory at the moment.

## Build, Test, and Development Commands
Use Emacs batch commands for quick validation:

- `emacs -Q --batch -L . --eval "(progn (require 'package) (package-initialize))" -f batch-byte-compile overleaf-project.el` byte-compiles the package and catches warnings when dependencies are installed.
- `emacs -Q --batch -L . --eval "(progn (require 'package) (package-initialize))" --eval "(require 'overleaf-project)"` is a simple load smoke test.
- `emacs -Q -L . --eval "(progn (require 'package) (package-initialize))"` then `M-x overleaf-authenticate`, `M-x overleaf-project-clone`, `M-x overleaf-project-push`, and `M-x overleaf-project-pull` is the practical manual path for testing auth and sync flows.

GitHub Actions runs Melpazoid checks on every push and pull request; use that workflow as the packaging baseline.

## Coding Style & Naming Conventions
Follow idiomatic Emacs Lisp: keep `lexical-binding: t`, use Emacs default indentation, and write docstrings for public functions and variables. Public APIs and user options should use the `overleaf-` prefix; generic internal helpers use `overleaf--` and repo/workflow internals use `overleaf-project--`. Prefer `defcustom` for user-facing configuration, `defvar` for cached process state, and `cl-defstruct` for structured data passed between helpers. Keep comments brief and only where the control flow is not obvious.

## Testing Guidelines
No committed automated test suite exists yet, so validate changes with byte-compilation and a manual Overleaf session. For sync-related work, check clone, push, pull, pending-state recovery, and Git-based conflict branch resolution. For cookie handling changes, test webdriver authentication and any documented manual cookie persistence setup.

## Commit & Pull Request Guidelines
Recent history favors short, imperative subjects, often with prefixes such as `feat:`, `fix:`, `docs:`, `refactor:`, `chore:`, and `tidy:`. Keep commits focused and avoid mixing behavior changes with unrelated asset or documentation churn. Pull requests should summarize user-visible impact, list manual validation steps, link any related issue, and include screenshots or a short GIF for UI-facing changes.

## Security & Configuration Tips
Do not commit real Overleaf cookies, project identifiers, ancestor backup files, or debug logs containing session data. Redact examples in docs and issues before sharing them.
