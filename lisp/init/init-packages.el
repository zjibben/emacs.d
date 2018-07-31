;; init-packages
;;
;; initialize repositories and packages

;; add repos
;; LANL proxy doesn't like some https, so need to use http when behind it
(add-to-list 'package-archives
             `("melpa" . ,(if proxy-enable "http://melpa.org/packages/"
                            "https://melpa.org/packages/")))

;; install missing packages
(require 'init-install-packages)

;; configure installed packages
;; AUCTeX
(setq         TeX-auto-save   t
              TeX-parse-self  t)
(setq-default TeX-master      nil
              TeX-engine     'xetex)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'init-packages)
