;;
;; init-packages
;;
;; initialize repositories and packages
;;
;; zjibben <threeofsix@gmail.com>

;; add repos
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;;("marmalade" . "https://marmalade-repo.org/packages/")
                         ))
(package-initialize)

;; install missing packages
(require 'init-install-packages)

;; configure installed packages
;; AUCTeX
(setq         TeX-auto-save   t
              TeX-parse-self  t)
(setq-default TeX-master      nil
              TeX-engine     'xetex)
(require 'latex-pretty-symbols)

(pdf-tools-install t nil t)

;; plantuml-mode
(setq-default plantuml-jar-path "/opt/plantuml/plantuml.jar")   ;arch
;(setq-default plantuml-jar-path "/usr/share/java/plantuml.jar") ;fedora

;; eimp (fit images to window by default)
(add-hook 'image-mode-hook  'eimp-mode)
;(add-hook 'eimp-mode-hook  'eimp-fit-image-to-window)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; arduino-mode
(defun arduino-compile ()
  "Compile Arduino program and upload."
  (interactive)
  (compile "make upload")
  (pop-to-buffer "*compilation*"))

;; gpg stuff
(pinentry-start)

;; mu4e
;; mu needs to be separately installed, so don't fail if mu4e isn't found
;; system/user-dependent material is initialized in init-private-info.el
(if (string-match ".*\.lanl\.gov" system-name)
    (add-to-list 'load-path "~/opt/mu/share/emacs/site-lisp/mu4e"))
(require 'mu4e nil t)
(require 'mu4e-contrib nil t)
(setq mu4e-html2text-command 'mu4e-shr2text
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval  600
      message-send-mail-function 'smtpmail-send-it)

(provide 'init-packages)
