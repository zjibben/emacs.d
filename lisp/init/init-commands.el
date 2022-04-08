;; -*- lexical-binding: t -*-
;;
;; init-commands
;;
;; initialize my own commands

;(require 'cl-lib)

;; add a bunch of elements to a list
(defun add-all-to-list (list &rest elements)
  "Add many elements to a list."
  (dolist (element elements) (add-to-list list element)))

(defun hook-from-mode (mode)
  "Return the hook for a given mode."
  (intern (concat (symbol-name mode) "-hook")))

(defun add-to-mode-hooks (modes func)
  "Add a function to many mode hooks."
  (dolist (mode modes) (add-hook (hook-from-mode mode) func)))

(defun modes-set-key (modes key function)
  "Set keyboard shortcut in modes provided in a list"
  (dolist (mode modes) (mode-set-key (hook-from-mode mode) key function)))

(defun mode-set-key (mode-hook key function)
  "Set keyboard shortcut in a given mode."
  (add-hook mode-hook `(lambda () (local-set-key ,key ',function))))

(defun mode-unset-key (mode-hook key)
  "Set keyboard shortcut in a given mode."
  (add-hook mode-hook `(lambda () (local-unset-key ,key))))

(defmacro setq-mode-default (mode variable value)
  "Set a variable default in a given mode."
  `(add-hook (hook-from-mode ,mode) (lambda () (setq ,variable ,value))))

(defun system-distro () (car (split-string (shell-command-to-string "lsb_release -si") "\n")))

;; print the result of a shell command to a string,
;; but do so from an interactive login shell that
;; gets the user's full environment, and remove any
;; junk that might have also been printed upon logging in.
(defun full-shell-command-to-clean-string (command)
  (car (last
        (split-string
         (shell-command-to-string (concat "$SHELL --login -i -c '" command "'"))
         "\n")
        2)))

;; interactive commands

(defun create-tags (dir-name)
  "Create tags file in specified directory, from source files in subdirectories."
  (interactive "DDirectory: ")

  ;; remove trailing "/" if one exists, then create tags in the given directory
  (let ((dir (directory-file-name dir-name)))
    (or
     ;; attempt to use ctags
     (eql (shell-command (format "cd %s && ctags -eRf TAGS" dir)) 0)

     ;; if ctags isn't found, use etags
     ;; case insensitive, including .f .f90 .c .h .cu .cl files (add .py?)
     (shell-command
      (format "find %s -type f -iname \"*.f90\" -o -iname \"*.[fch]\" -o -iname \"*.c[lu]\" \\
            | etags - -o %s/TAGS" dir dir)))))

(defun create-shell ()
  "Open a new shell buffer."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

(require 'python)
(defun create-python-shell ()
  "Open a new Python shell buffer."
  (interactive)

  ;; for some reason, if the current buffer is a python shell, creating a new
  ;; shell screws up existing ones. switch to temp buffer before making the new one.
  (with-temp-buffer
    (pop-to-buffer
     (python-shell-make-comint
      (python-shell-calculate-command)
      (generate-new-buffer-name "*python*")
      t))
    ;; rename the buffer after the fact, because Emacs's internal commands for
    ;; making Python shells do their own manipulation on any buffer name you hand it
    (rename-buffer (generate-new-buffer-name "*python*"))))

;; quick macro directly to snoonet
(defun irc-snoonet ()
  "Connect to snoonet"
  (interactive)
  (erc :server   "irc.snoonet.org"
       :port     6667
       :nick     irc-snoonet-user
       :password irc-snoonet-pass))

;; function for compiling in a directory,
;; then storing that given directory for quick recompiles
(let ((compile-in-dir--dir nil)) ; stored compile directory
  (defun compile-in-dir (&optional get-new-directory)
    "Issues a compile in a given directory or recompiles
in last given directory. If given a non-nil argument
(or prefixed with C-u), it asks for a directory."
    (interactive "P")

    (if (or get-new-directory (not compile-in-dir--dir))
        ;; the default-directory variable is used internally in 'compile
        (let ((default-directory (setq compile-in-dir--dir (read-directory-name "In directory: ")))
              (comint-terminfo-terminal "dumb")
              )
          (call-interactively 'compile))
      (recompile))

    (pop-to-buffer "*compilation*")))

(defun create-org-log ()
  "Add an org log timestamp at point."
  (interactive)
  (org-insert-drawer nil "LOGBOOK")
  (insert "- Created ")
  (org-insert-time-stamp (current-time) nil t)
  (org-up-element)
  (org-up-element)
  (org-cycle)
  (next-line)
  (org-return-indent)
  (org-open-line 1))

(provide 'init-commands)
