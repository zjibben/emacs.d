;;
;; init-packages
;;
;; initialize repositories and packages
;;
;; zjibben <threeofsix@gmail.com> 12/2015
;; 

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

;; multi-term
;; (require 'multi-term)
;; (setq multi-term-program "/bin/bash")
;; ;; TODO: handle C-v as page down, but also make a way to send ^V to the terminal
;; (setq term-unbind-key-list (append term-unbind-key-list (list
;;                                                          "C-p"
;;                                                          "C-n"
;;                                                          "C-f"
;;                                                          "C-b"
;;                                                          "C-/"
;;                                                          "C-l"
;;                                                          )))
;; (setq term-bind-key-alist (list
;;                            (cons "M-."  'term-send-raw-meta)
;;                            (cons "M-p"  'term-send-up)
;;                            (cons "M-n"  'term-send-down)
;;                            (cons "M-r"  'term-send-reverse-search-history)
;;                            ;;(cons "<f6>" 'term-send-raw)
;;                            ))
;; (setq term-bind-key-alist (append term-bind-key-alist
;;                                   (list
;;                                    (cons "M-."  'term-send-raw-meta)
;;                                    )))

;; (add-to-list 'term-unbind-key-list '(list
;;                                      "C-p"))
;; (setq term-unbind-key-list (append term-unbind-key-list (list
;;                                                          "C-p"
;;                                                          "C-n"
;;                                                          "C-f"
;;                                                          "C-b"
;;                                                          "C-/"
;;                                                          "C-l"
;;                                                          "C-v"
;;                                                          "M-v"
;;                                                          )))
;;(setq term-unbind-key-list (append term-unbind-key-list (list "C-b" )))
;; (setq term-bind-key-alist (list
;;                            (cons "C-p"  'previous-line)
;;                            (cons "C-n"  'next-line)
;;                            (cons "C-f"  'forward-char)
;;                            ;;(cons "C-b"  'backward-char)
;;                            (cons "C-l"  'recenter-top-bottom)
;;                            (cons "C-/"  'undo)
;;                            (cons "C-v"  'scroll-up-command)
;;                            (cons "M-v"  'scroll-down-command)
;;                            (cons "M-."  'term-send-raw-meta)
;;                            (cons "M-p"  'term-send-up)
;;                            (cons "M-n"  'term-send-down)
;;                            (cons "M-r"  'term-send-reverse-search-history)
;;                            ;;(cons "<f6>" 'term-send-raw)
;;                            ))
;; (setq term-bind-key-alist (append term-bind-key-alist
;;                                   (list
;;                                    (cons "M-."  'term-send-raw-meta)
;;                                    )))

(provide 'init-packages)
