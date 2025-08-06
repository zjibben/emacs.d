;; my emacs configuration file

(add-to-list 'load-path "~/.emacs.d/lisp/init/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-secrets nil t)
(require 'init-commands)
(require 'init-packages)
(require 'init-config)
(require 'init-theme)
