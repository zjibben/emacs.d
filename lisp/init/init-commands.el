;;
;; init-commands
;;
;; initialize a few commands I use
;;
;; zjibben <threeofsix@gmail.com> 12/2015
;; 

;; add a bunch of elements to a list
(defun add-all-to-list (list &rest elements)
  "Add many elements to a list."
  (dolist (element elements) (add-to-list list element)))

;; update tags file 
(defun create-tags (dir-name)
  "Create tags file in specified directory, from source files in subdirectories."
  (interactive "DDirectory: ")

  ;; remove trailing "/" if one exists, then create tags in the given directory
  (let ((dir (directory-file-name dir-name)))
    (or
     ;; attempt to use ctags
     (eql (shell-command (format "ctags -f %s/TAGS -e -R" dir)) 0)

     ;; if ctags isn't found, use etags
     ;; case insensitive, including .f .f90 .c .h .cu .cl files (add .py?)
     (shell-command 
      (format "find %s -type f -iname \"*.f90\" -o -iname \"*.[fch]\" -o -iname \"*.c[lu]\" \\
            | etags - -o %s/TAGS" dir dir))
     )))

;; create a new shell buffer
(defun create-shell ()
  "Open a new shell."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

;; irc
(require 'init-login-info nil t) ;; attempt to grab login info
(defun irc-snoonet ()
  "Connect to snoonet"
  (interactive)
  (erc :server   "irc.snoonet.org"
       :port     6667
       :nick     irc-snoonet-user
       :password irc-snoonet-pass ))

(provide 'init-commands)