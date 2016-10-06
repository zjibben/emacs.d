;;
;; emacs configuration file
;;
;; zjibben <threeofsix@gmail.com>

(add-to-list 'load-path "~/.emacs.d/lisp/init/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq proxy-enable nil)
(require 'init-private-info nil t) ;; attempt to grab private info
(when proxy-enable (require 'init-proxy))
(require 'init-commands)
(package-initialize)
(require 'init-packages)
(require 'init-config)
(require 'init-theme)
