;;
;; compile-in-dir
;;
;; function for compiling/recompiling in a directory
;;
;; zjibben <threeofsix@gmail.com>

;; execute a command in a different directory
;; inspired by http://www.emacswiki.org/emacs/CompileCommand#toc7
(defun in-directory (&optional command dir)
  "Runs execute-extended-command with default-directory set to the given directory."
  (interactive)

  ;; if called from a script and the directory is nil, ask the user interactively
  (or dir (setq dir (read-directory-name "In directory: ")))

  (let ((default-directory dir))
    (call-interactively (or command 'execute-extended-command))))

;; compile in a directory/recompile previous compile command
(defvar compile-in-dir-occurred nil
  "Indicates to compile-in-dir whether a recompile can be performed.")
(defun compile-in-dir (&optional force-compile)
  "Issues a compile (first time) or recompile.
   If given a non-nil argument (or prefixed with C-u), it issues a compile."
  (interactive "P")

  ;; force a compile if we haven't compiled yet
  (or force-compile (setq force-compile (not compile-in-dir-occurred)))

  ;; either compile or recompile
  (if force-compile
      (in-directory 'compile)
    (recompile))

  ;; move the point to the compilation buffer
  (pop-to-buffer "*compilation*")

  ;; note for later that we have compiled in this session and can now recompile
  (setq compile-in-dir-occurred t))

(provide 'compile-in-dir)
