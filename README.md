# About

My Emacs configuration. I keep fairly true to the Emacs defaults, only modifying
the look and adding a few shortcuts and commands, without doing anything too
drastic.

It's important to note for anyone else who might be interested in using it that
environment and certain mode settings in `init-config.el` are very specific to
my own machines and projects.

## Features

### Key Shortcuts

| Key Sequence                | Function                                                                             |
| --------------------------- | ------------------------------------------------------------------------------------ |
| `<f1>`                      | Open a shell.                                                                        |
| `<f2>`                      | Open an IPython shell.                                                               |
| `M-o`, `M-O`                | Move to the next/previous window.                                                    |
| `C-c C-c` in C, C++, and F90 modes | Compile or recompile a project. Preface with `C-u` to choose its directory.  |
| `C-s`, `C-r`                | Do a regexp search                                                                   |
| `C-x C-b`                   | Open ibuffer                                                                         |
| `C-x C-k`                   | Kill this buffer with no questions asked                                             |
| `C-c g`                     | Open Magit's file dispatch                                                           |

### Notable Packages

- adaptive-wrap
- auctex
- clang-format
- counsel and Ivy
- gptel and aidermacs
- lsp-mode
- magit
- markdown-mode
- org-roam
- pdf-tools
- powerline
- vterm

### Behavior

The theme is set dynamically whenever a new frame is created. Theme selection,
fonts, and terminal-specific behavior live in `init-theme.el`; `ample-theme`
remains part of that setup.

`display-fill-column-indicator-mode` is automatically enabled in F90, C, C++,
Python, Emacs Lisp, Shell, and HTML modes. It follows the 100-column default.

LaTeX, Markdown, and Org modes enable spell checking. Org and LaTeX also render
many LaTeX macros, such as `\alpha`, as their corresponding characters.

## Dependencies

Emacs 29 or later is recommended; it includes the `use-package` support needed
to bootstrap the configuration and the built-in fill-column indicator. Package
dependencies are installed automatically from GNU ELPA and MELPA on first use.

The following command-line tools enable the corresponding optional workflows:

- `clang-format` for C++ formatting
- `clangd` for C++ LSP support
- `ipython3` for the Python REPL
- `pandoc` for Markdown export and preview
- TeX Live and Poppler for TeX and PDF workflows
- Git for Magit
- PlantUML for diagram rendering

# Installation

Clone the repository to `~/.emacs.d`:

``` shell
git clone https://github.com/zjibben/emacs.d.git ~/.emacs.d
```

Then, open Emacs. It will automatically download package dependencies and
configure them.

Optional private settings are loaded from `lisp/init/init-secrets.el`. This file
is not part of the repository. It is useful for credentials used by optional AI
integrations; omit it if those integrations are not used.

``` emacs-lisp
;; Define only the credentials for services you use.
(defun openrouter-api-key () "...")
(defun lanl-ai-portal-api-key () "...")

(provide 'init-secrets)
```

# Usage

This setup is intended to run as an Emacs daemon. For graphical clients, use

``` shell
emacsclient -c -a ""
```

For terminal clients, this shell function safely forwards filenames to the
daemon:

``` shell
emc() { emacsclient -t -a "" "$@"; }
```

For quick standalone terminal edits, use

``` shell
alias eml='emacs -nw -q -l ~/.emacs.d/init-lite.el'
```

to open a new Emacs session with the lite configuration.
