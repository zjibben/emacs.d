;;
;; emacs configuration file
;;
;; zjibben <threeofsix@gmail.com> 12/2015
;;

(add-to-list 'load-path "~/.emacs.d/lisp/init/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; (require 'init-proxy)
(require 'init-packages)
(require 'compile-in-dir)
(require 'init-commands)
(require 'init-config)
(require 'init-theme)
