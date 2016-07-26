;; compile-in-dir
;;
;; function for compiling in a directory,
;; then storing that given directory for quick recompiles
;;
;; zjibben <threeofsix@gmail.com>

(lexical-let ((compile-in-dir--dir nil)) ; stored compile directory
  (defun compile-in-dir (&optional get-new-directory)
    "Issues a compile in a given directory or recompiles
in last given directory. If given a non-nil argument
(or prefixed with C-u), it always asks for the directory."
    (interactive "P")

    (if (or get-new-directory (not compile-in-dir--dir))
        ;; the default-directory variable is used internally in 'compile
        (let ((default-directory (setq compile-in-dir--dir (read-directory-name "In directory: "))))
          (call-interactively 'compile))
      (recompile))

    (pop-to-buffer "*compilation*")))

(provide 'compile-in-dir)
