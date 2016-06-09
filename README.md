# About

My Emacs configuration. I keep fairly true to the Emacs defaults, only modifying the look and adding a few shortcuts and commands, without doing anything too drastic.

It's important to note for anyone else who might be interested in using it that environment and certain mode settings in `init-config.el` are very specific to my own machines and projects.

## Features

### Key Shortcuts

| Key Sequence                | Function                                                                             |
| --------------------------- | ------------------------------------------------------------------------------------ |
| `<f1>`                      | Open a shell.                                                                        |
| `<f2>`                      | Open an IPython shell.                                                               |
| `M-o`, `M-O`                | Move to the next/previous window.                                                    |
| `C-c C-c` in `fortran-mode` | Compile or recompile your project. Preface with `C-u` to reset your compile command. |
| `C-s`, `C-r`                | Do a regexp search                                                                   |
| `C-x C-b`                   | Open ibuffer                                                                         |
| `C-x C-k`                   | Kill this buffer with no questions asked                                             |
| `C-x g`                     | Open `magit-status`                                                                  |

### Outside Packages

- adaptive-wrap
- arduino-mode
- auctex
- djvu
- eimp
- fill-column-indicator
- gnuplot-mode
- haskell-mode
- latex-pretty-symbols
- magit
- pdf-tools
- pkgbuild-mode
- plantuml-mode
- powerline
- python-info
- rust-mode
- smex

### Behavior

The theme is set dynamically whenever a new frame is created. In particular, ample-theme is used in all cases, but if opened in a terminal your terminal's background color setting takes priority. Powerline's default theme is used for graphical frames and the vim theme is used for console frames. Font size is set based on screen resolution.

`fci-mode` is automatically enabled in F90, C, C++, Python, Emacs Lisp, Shell, and Arduino modes. It will show a line along the 101st column.

LaTeX and Org modes automatically spell check and display LaTeX macros like `\alpha` as their corresponding characters.

## Dependencies

For the most basic usage, the only dependency is Emacs. However, use of individual packages require their dependencies. For everything, these are required:

- texlive
- ipython3
- poppler
- git
- plantuml
- gnuplot
- haskell
- rust

# Installation

Clone the repository to `~/.emacs.d`:

    $ git clone git://github.com:zjibben/emacs.d.git ~/.emacs.d

Then, open Emacs. It will automatically download package dependencies and configure them.

If you need to go through a proxy, the setup will fail unless you set up your proxy through an `init-private-info.el` package. Private and user-specific login information is also set there. An example is shown below. If you don't need any of this, you can safely ignore it.

    ;; proxy info
    (defvar http-proxy-host  "proxyout.work.com" "Host address for http proxy")
    (defvar http-proxy-port  8080                "Host port for http proxy")
    (defvar no-proxy-domain  "work\\.com"        "Domain for which no proxy is needed")
    (defvar https-proxy-host http-proxy-host     "Host address for https proxy")
    (defvar https-proxy-port http-proxy-port     "Host port for https proxy")
    (setq-default proxy-enable t)
    
    ;; personal/login info
    (setq-default user-mail-address "me@somewhere.com"
                  irc-snoonet-user "username"
                  irc-snoonet-pass "password")
    
    (provide 'init-private-info)

# Usage

This setup does not make any efforts to reduce startup time, since I almost always run Emacs as a daemon anyways. In particular, I have

    emacsclient -c -a ""

assigned to a keyboard shortcut through my desktop environment, and

    function emc { emacsclient -t -a "" -e "(unless (string= \"\" \"$@\") (find-file \"$@\"))"; }
    export -f emc

in my `~/.bashrc` for console use. This function uses Elisp to permanently add any named files to the buffer list, so they are not closed when the Emacs client is closed. For quick edits, I use

    alias eml='emacs -nw -q -l ~/.emacs.d/init-lite.el'

to open a new Emacs session with the lite configuration, which loads rather quickly.
