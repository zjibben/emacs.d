;; my emacs configuration file

;; ;; startup profiling
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-to-list 'load-path "~/.emacs.d/lisp/init/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq-default use-package-always-ensure t
              use-package-always-defer t)

(require 'init-secrets nil t)
(require 'init-commands)
(require 'init-theme)
(require 'init-packages)
(require 'init-config)
