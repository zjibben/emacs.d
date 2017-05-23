;;
;; init-config
;;
;; initialize basic emacs configuration
;;
;; zjibben <threeofsix@gmail.com>

;; basic configuration
(setq-default display-time-default-load-average   nil     ; hide load average in modeline
              display-time-day-and-date           1       ; display both date and time
              indent-tabs-mode                    nil     ; spaces instead of tabs
              require-final-newline               t       ; ensure files end with a newline
              calc-angle-mode                     'rad    ; calc to radians-mode
              calc-multiplication-has-precedence  nil     ; sensible order of operations
              Info-fontify-maximum-menu-size      1000000 ; increase Info highlight limit
              shift-select-mode                   nil     ; shift/caps won't select text
              save-interprogram-paste-before-kill t       ; don't lose clipboard entries
              ido-everywhere                      t
              ido-enable-flex-matching            t
              ido-ignore-extensions               t       ; ido ignores extensions like '~' and '.o'
              ido-use-faces                       nil     ; use flx highlights
              completions-format                  'vertical ; sort along columns rather than rows
              show-paren-delay                    0       ; show matching parentheses immediately
              proced-auto-update-flag             t
              proced-auto-update-interval         2
              doc-view-resolution                 300
              
              ;; smooth scrolling
              scroll-step                1
              scroll-conservatively      10000
              mouse-wheel-scroll-amount '(1 ((shift) . 1)) )
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(ido-mode           1) ; enable ido-mode for switching buffers and finding files
(flx-ido-mode       1)
(display-time-mode  1) ; activate modeline time and date
(menu-bar-mode     -1) ; deactivate menubar
(tool-bar-mode     -1) ; deactivate toolbar
(scroll-bar-mode   -1) ; deactivate scroll bar
(show-paren-mode    1) ; activate matching parenthesis highlighting
(defalias 'yes-or-no-p 'y-or-n-p) ; I don't like typing 2 or 3 characters when I can type 1

;; open shells in current window
(add-to-list 'display-buffer-alist '("^\\*shell\\*$" . (display-buffer-same-window)))

;; default programs
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args (concat "--no-confirm-exit -i"
                                            (if (>= (string-to-number
                                                     (shell-command-to-string "ipython3 --version"))
                                                    5)
                                                " --simple-prompt")))
(when (string= (system-distro) "Fedora") (setq gnuplot-program "/usr/bin/gnuplot-qt"))

;; ensure environment is consistent with login environment (ref http://stackoverflow.com/a/6415812)
(let ((path-from-shell (full-shell-command-to-clean-string "echo $PATH")))
  (setenv "PATH" path-from-shell)
  (setq eshell-path-env path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version)) ; needed by pinentry

;; workstation-specific settings
(when (or (string= system-name "annapurna") (string= system-name "erdelyi.lanl.gov"))
  (setenv "PATH" (concat
                  "/opt/intel/bin"
                  (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (concat
                             "/opt/intel/lib/intel64/:"
                             "/opt/openmpi/1.6.5-intel14/lib/:"
                             (getenv "LD_LIBRARY_PATH"))))

;; fortran settings
(setq-default f90-do-indent           2
              f90-if-indent           2
              f90-type-indent         2
              f90-program-indent      2
              f90-continuation-indent 4)
(add-to-list 'completion-ignored-extensions ".mod")

;; (use-package f90
;;   :mode ("\\.\\(F90\\|fpp\\)$" . f90-mode)
;;   :config
;;   (setq-default f90-do-indent           2
;;                 f90-if-indent           2
;;                 f90-type-indent         2
;;                 f90-program-indent      2
;;                 f90-continuation-indent 4)
;;   (add-hook 'f90-mode-hook 'fci-mode)
;;   (mode-set-key 'f90-mode-hook (kbd "C-c C-c") 'compile-in-dir)
;;   )

;; line length settings
(setq-default fill-column 101)
(setq-default fci-rule-color "dim gray")
(add-to-mode-hooks '(f90 c c++ python emacs-lisp sh arduino) 'fci-mode)
(add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(setq TeX-view-program-selection '((output-pdf "pdf-tools")))
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

;; mode settings
;; is there a way to make C-e go to the actual end of the line in visual-line-mode??
(add-hook 'LaTeX-mode-hook 'flyspell-mode)    ; auto spell-checking in latex
;;(add-hook 'LaTeX-mode-hook 'visual-line-mode) ; break lines between words

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

(add-hook 'org-mode-hook 'flyspell-mode)             ; auto spell-checking in org
(add-hook 'org-mode-hook 'visual-line-mode)          ; break lines between words
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode) ; wraped headers are indented properly
(setq org-pretty-entities t)                         ; render math by default

;; set file ending defaults
(add-all-to-list 'auto-mode-alist
                 '("\\.\\(F90\\|fpp\\)$" . f90-mode   )
                 '("\\.\\(cu\\|cl\\)$"   . c-mode     )
                 '(".m$"                 . octave-mode)
                 '(".pdf$"               . pdf-view-mode))

;; keybindings
;; note: can put these all into a minor mode to group them together and deactivate easily,
;;       as well as easily override major mode settings. Follow directions here:
;;       http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;       for now, just manually override major mode settings that don't respect these
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "<f1>") 'create-shell)
(global-set-key (kbd "<f2>") 'create-python-shell)
(global-set-key (kbd "C-x g") 'magit-status)

;; could put these mode-specific types of shortcuts into a minor mode as well
(modes-set-key '(f90 c c++ arduino) (kbd "C-c C-c") 'compile-in-dir)
(mode-unset-key 'ibuffer-mode-hook (kbd "M-o")) ; instead of minor mode, manually overriding for now

(provide 'init-config)
