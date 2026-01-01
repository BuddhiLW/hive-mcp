# Contributing to emacs-mcp

## Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/BuddhiLW/emacs-mcp.git
   cd emacs-mcp
   ```

2. Start nREPL for Clojure development:
   ```bash
   clojure -M:nrepl  # Port 7910
   ```

3. Load the Emacs package:
   ```elisp
   (add-to-list 'load-path "/path/to/emacs-mcp/elisp")
   (require 'emacs-mcp)
   (emacs-mcp-mode 1)
   ```

## Running Tests

```bash
clojure -M:test
```

## MELPA Submission

The package is submitted to MELPA with the following recipe:

```elisp
(emacs-mcp :fetcher github
           :repo "BuddhiLW/emacs-mcp"
           :files ("elisp/*.el"))
```

### Submission Steps

1. Fork [melpa/melpa](https://github.com/melpa/melpa)
2. Add recipe to `recipes/emacs-mcp`
3. Test locally:
   ```bash
   make recipes/emacs-mcp
   ```
4. Submit PR to MELPA repository

### Package Requirements

- [x] Proper headers (Author, Maintainer, URL, Keywords, Package-Requires)
- [x] SPDX-License-Identifier
- [x] `lexical-binding: t`
- [x] `;;;###autoload` cookies on public functions
- [x] Ends with `(provide 'emacs-mcp)` and `;;; emacs-mcp.el ends here`

## Code Style

- Follow [Emacs Lisp conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)
- Use `emacs-mcp-` prefix for all public symbols
- Use `emacs-mcp--` prefix for private symbols
- Document all public functions with docstrings

## Commit Messages

Use conventional commits:
- `feat:` New features
- `fix:` Bug fixes
- `docs:` Documentation changes
- `chore:` Maintenance tasks
- `refactor:` Code restructuring

## Pull Requests

1. Create a feature branch from `main`
2. Make your changes
3. Test thoroughly
4. Submit PR with clear description

## Releasing

This is a fast-moving project. Create a new release after each PR merge to main.

### Version Format: `v0.X.Y`

| Change Type | Version Update | Example |
|-------------|----------------|---------|
| **Milestone** (significant feature set) | Bump X | v0.3.0 → v0.4.0 |
| **Feature or fix** | Bump Y | v0.3.0 → v0.3.1 |

### Release Process

1. Merge PR to `main`
2. Create release with changelog:
   ```bash
   gh release create v0.X.Y --target main \
     --title "v0.X.Y - Brief description" \
     --notes "## What's New\n\n- Feature 1\n- Fix 1"
   ```
3. Include link to full changelog:
   ```
   https://github.com/BuddhiLW/emacs-mcp/compare/v0.X.Z...v0.X.Y
   ```

### Current Version

See [Releases](https://github.com/BuddhiLW/emacs-mcp/releases) for the latest version.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
