(define-derived-mode tikz-mode plain-tex-mode "TikZ"
  "Major mode for editing TikZ files.

Main command is C-c to rebuild your TikZ file. A preview buffer
will be automatically opened. You can set a preamble in the string
tikz-preamble."
  (setq-local indent-line-function 'latex-indent)
  (define-key (current-local-map) "\C-c\C-c" 'tikz-compile))

(defvar tikz-preamble
  ''
  "TikZ preamble"
  )

(defvar tikz-work-dir
  "/tmp/tikz"
  "Directory where TikZ builds preview"
  )

(defun tikz-compile ()
  (interactive)

  (let ((fname (file-name-nondirectory (buffer-file-name))))
    (let ((work-dir (concat tikz-work-dir "/" fname))
          (work-file (concat tikz-work-dir "/" fname "/" fname)))
      (make-directory work-dir t)
      (shell-command (concat "echo '\\documentclass[tikz]{standalone}' > " work-file " && "
                             "echo '" tikz-preamble "' >> " work-file " && "
                             "echo '\\begin{document}' >> " work-file " && "
                             "cat " (buffer-file-name) " >> " work-file " && "
                             "echo '\\end{document}' >> " work-file))
      (shell-command (concat "cd " work-dir
                             " && pdflatex -interaction batchmode -file-line-error " fname))
      (find-file-noselect (concat (file-name-sans-extension work-file) ".pdf") t))))
