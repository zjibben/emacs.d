;;
;; init-commands
;;
;; initialize a few commands I use
;;
;; zjibben <threeofsix@gmail.com> 12/2015
;; 

;; update tags using etags, including .F .f .c .C .h .H .F90 .f90 .cu .cl files (add .py?)
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command 
;;    (format "find %s -type f -iname \"*.f90\" -o -iname \"*.[fch]\" -o -iname \"*.c[lu]\" \\
;;             | etags - -o %s/TAGS" dir-name dir-name)))

;; update tags using ctags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R" (concat (directory-file-name dir-name) "/TAGS") )))

;; create a new shell buffer
(defun create-shell ()
  "Open a new shell."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

;; irc
(require 'init-login-info)
(defun irc-snoonet ()
  "Connect to snoonet"
  (interactive)
  (erc :server   "irc.snoonet.org"
       :port     6667
       :nick     irc-snoonet-user
       :password irc-snoonet-pass ))

(provide 'init-commands)
