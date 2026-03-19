# AGENTS.md - Emacs Configuration Repository Guide

This document provides guidance for agentic coding tools working in this Emacs Lisp configuration repository.

## Project Overview

This is a personal Emacs configuration ("dotfiles") based on Centaur Emacs. It uses a modular architecture with feature-specific initialization files.

## Project Structure

```
~/.emacs.d/
├── init.el              # Entry point - loads all modules
├── custom.el            # User customizations (git-tracked defaults)
├── custom-post.el       # Personal overrides (optional, not tracked)
├── lisp/
│   ├── init-const.el    # System/platform constants
│   ├── init-custom.el   # Customization group definitions
│   ├── init-package.el  # Package management setup
│   ├── init-base.el     # Core Emacs settings
│   ├── init-funcs.el    # Utility functions
│   ├── init-lsp.el      # LSP configuration
│   ├── init-*.el        # Other feature modules
│   └── extensions/      # Third-party packages bundled locally
└── snippets/            # Yasnippet snippets
```

## Build/Lint/Test Commands

### Loading the Configuration (Basic Test)

```bash
# Full config test (batch mode)
emacs -q --batch \
  --eval "(message \"Testing...\")" \
  --eval "(let ((early-init-file (locate-user-emacs-file \"early-init.el\"))
              (user-init-file (locate-user-emacs-file \"init.el\")))
          (and (>= emacs-major-version 27) (load early-init-file))
          (load user-init-file))" \
  --eval "(message \"Testing...done\")"

# Minimal config test (for troubleshooting)
emacs -Q -l ~/.emacs.d/init-mini.el
```

### Byte Compilation

```elisp
;; In Emacs
M-x byte-compile-file           ; Compile current buffer
M-x byte-recompile-directory    ; Recompile directory
M-x centaur-recompile           ; Custom command if available
```

### Package Archives

Default archive is `melpa`. Change in `custom.el`:
```elisp
(setq centaur-package-archives 'melpa)  ; Options: melpa, bfsu, iscas, netease, sjtu, tencent, tuna, ustc
```


### Evaluation Commands

```elisp
;; Evaluate current file
M-x eval-buffer

;; Evaluate region
M-x eval-region

;; Reload init.el
M-x load-file RET ~/.emacs.d/init.el

;; Byte compile current file
M-x byte-compile-file

;; Native compile (Emacs 29+)
M-x native-compile-async
```

### Linting

```bash
# Check with package-lint (if installed)
emacs --batch -f package-initialize -l package-lint.el -f package-lint-batch-and-exit lisp/*.el

# Byte compile to find errors
emacs --batch -l ~/.emacs.d/init.el -f batch-byte-compile lisp/*.el
```

### Check for Errors on Startup

```bash
# Start Emacs with debug on error
emacs --debug-init

# Time startup
emacs --timed-run -l ~/.emacs.d/init.el
```

## Coding Style Guidelines

### File Headers

Every `.el` file must have this header structure:

```elisp
;;; init-module.el --- Brief description.	-*- lexical-binding: t -*-

;; Copyright (C) 2020  Author Name

;; Author: Author Name <email@example.com>
;; Keywords: category

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Detailed description here.
;;

;;; Code:

;; ... code ...

(provide 'init-module)
;;; init-module.el ends here
```

### Lexical Binding

**ALWAYS** use lexical binding. Add `-*- lexical-binding: t -*-` on the first line.

### Requires

Use `eval-when-compile` for compile-time dependencies:

```elisp
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))
```

### Package Configuration with use-package

This configuration uses `use-package` extensively. Follow these patterns:

```elisp
;; Basic package setup
(use-package package-name
  :ensure t      ; Auto-install if missing
  :defer t       ; Lazy load
  :diminish      ; Hide from mode line
  :hook ((mode-name . hook-function))
  :bind (("C-c x" . command-name))
  :init
  ;; Code run before package loads
  (setq variable-name value)
  :config
  ;; Code run after package loads
  )
```

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Modules | `init-*.el` | `init-lsp.el` |
| Constants | `sys/` or `emacs/` prefix | `sys/macp`, `emacs/>=27p` |
| Custom group | `centaur-` prefix | `centaur-theme` |
| Functions | Namespace prefix | `yuf/`, `my-` |
| Custom variables | `centaur-` prefix | `centaur-proxy` |
| Private vars | Double hyphen | `--internal-var` |

### Variable Definitions

```elisp
;; Constants
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; Customizable variables
(defcustom centaur-theme 'default
  "Set color theme."
  :group 'centaur
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Dark theme" dark)))

;; Buffer-local variables
(defvar-local my-local-var nil
  "Buffer-local documentation.")
```

### Function Definitions

```elisp
(defun namespace/function-name (arg1 arg2)
  "Brief description.
Detailed explanation of what the function does.
ARG1 is the first argument.
ARG2 is the second argument."
  (interactive "p")  ; For interactive commands
  ;; Implementation
  )

;; For commands, include interactive spec:
(defun my-command ()
  "Do something useful."
  (interactive)
  (message "Done!"))
```

### Hooks

```elisp
;; Using :hook in use-package
(use-package package
  :hook ((prog-mode . my-hook-function)))

;; Traditional add-hook
(add-hook 'after-init-hook #'my-function)
```

### Key Bindings

```elisp
;; Using bind-keys macro
(bind-keys ("C-c f" . find-file)
           ("C-c s" . save-buffer))

;; Global key binding
(global-set-key (kbd "C-c j") 'my-jump-function)

;; Mode-specific
(define-key prog-mode-map (kbd "C-c c") 'compile)
```

### Advice Pattern

```elisp
;; Around advice
(defun my/advice-function (orig-fun &rest args)
  "Advice for ORIG-FUN with ARGS."
  (apply orig-fun args))
(advice-add 'target-function :around #'my/advice-function)
```

## Module Loading Order

From `init.el`, modules load in this order:

1. `init-const` - Platform constants
2. `init-custom` - Customization definitions
3. `init-package` - Package management
4. `init-base` - Core settings
5. `init-funcs` - Utility functions
6. Feature modules (completion, ui, lsp, languages, etc.)

**Important**: Add new modules to `init.el` in the appropriate location.

## Important Files to Know

| File | Purpose |
|------|---------|
| `init.el` | Entry point - add new module requires here |
| `init-custom.el` | Add new `defcustom` variables here |
| `init-const.el` | Add platform/Emacs version constants here |
| `init-funcs.el` | Add utility functions here |
| `custom.el` | User's custom-set-variables (tracked) |
| `custom-post.el` | Personal overrides (create if needed) |

## Common Tasks

### Add a New Package

1. Create `lisp/init-package-name.el` following the header template
2. Use `use-package` to configure it
3. Add `(require 'init-package-name)` to `init.el`
4. Byte-compile: `M-x byte-compile-file`

### Add a New Function

1. Add to `lisp/init-funcs.el` if general-purpose
2. Add to the relevant `init-*.el` if package-specific
3. Use `defalias` for shorter command names

### Modify User Settings

User customization values go in `custom.el` (tracked) or `custom-post.el` (not tracked, for machine-specific settings).

## Platform Detection

Available platform predicates from `init-const.el`:

- `sys/win32p` - Windows
- `sys/linuxp` - Linux
- `sys/macp` - macOS
- `sys/mac-x-p` - macOS with GUI
- `emacs/>=27p` - Emacs version checks

## Git Commit Convention

This repository follows [Conventional Commits](https://www.conventionalcommits.org/) specification.

### Format

```
<type>(<scope>): <description>

[optional body]

[optional footer(s)]
```

### Types

| Type | Description | Example |
|------|-------------|---------|
| `feat` | New feature | `feat(lsp): add java debug support` |
| `fix` | Bug fix | `fix(base): improve exec-path-from-shell condition` |
| `refactor` | Code refactoring (no feature/fix) | `refactor(highlight): clean up and enable indent guides` |
| `style` | Code style changes (formatting, whitespace) | `style(v): use sharped quote for lambda` |
| `perf` | Performance improvement | `perf: optimize projectile and ivy for faster file finding` |
| `docs` | Documentation changes | `docs: update README with new keybindings` |
| `chore` | Maintenance tasks, dependency updates | `chore: update submodules` |
| `test` | Adding/updating tests | `test: add unit tests for proxy functions` |

### Scopes

Scope is optional but recommended. Common scopes in this repo:

- `base` - Core Emacs settings (`init-base.el`)
- `lsp` - LSP configuration (`init-lsp.el`)
- `completion` - Completion stack (`init-completion.el`)
- `project` - Project management (`init-project.el`)
- `org` - Org mode configuration (`init-org.el`)
- `highlight` - Highlighting features (`init-highlight.el`)
- `v` - Personal/utility functions (`init-funcs.el`)

### Examples from Repository History

```bash
feat(project): migrate from projectile to built-in project.el
feat(completion): migrate to vertico/consult/corfu stack
feat: use nerd-icons instead of all-the-icons
fix(base): improve exec-path-from-shell condition
refactor(awesome-pair): simplify hook setup
refactor: extract magit config to separate file
style(v): use sharped quote for lambda
perf: optimize projectile and ivy for faster file finding
```

### Guidelines

1. **Use imperative mood**: "add feature" not "added feature"
2. **Lowercase description**: `feat: add new function` not `feat: Add new function`
3. **No period at end**: `feat: add function` not `feat: add function.`
4. **Keep first line under 72 characters**
5. **Scope should be the module name** (without `init-` prefix): `lsp` not `init-lsp`

## Error Recovery

If Emacs fails to start:

```bash
# Start with minimal config
emacs -q

# Debug startup errors
emacs --debug-init

# Skip init file
emacs -q -l ~/.emacs.d/init.el
```
