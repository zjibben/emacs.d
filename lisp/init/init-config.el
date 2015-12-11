;;
;; init-config
;;
;; initialize basic emacs configuration
;;
;; zjibben <threeofsix@gmail.com> 12/2015
;; 

;; basic configuration
(setq-default display-time-default-load-average  nil      ;; hide load average in modeline
              display-time-day-and-date          1        ;; display both date and time
              indent-tabs-mode                   nil      ;; spaces instead of tabs
              calc-angle-mode                    'rad     ;; calc to radians-mode
              calc-multiplication-has-precedence nil      ;; sensible order of operations
              Info-fontify-maximum-menu-size     1000000  ;; increase Info highlight limit
              shift-select-mode                  nil      ;; shift/caps won't select text
              ido-everywhere                     t
              ido-enable-flex-matching           t
              )
(ido-mode           1)  ;; enable ido-mode for switching buffers and finding files (replace with helm?)
(display-time-mode  1)  ;; activate modeline time and date
(menu-bar-mode     -1)  ;; deactivate menubar
(tool-bar-mode     -1)  ;; deactivate toolbar
(scroll-bar-mode   -1)  ;; deactivate scroll bar
(show-paren-mode    1)  ;; activate matching parenthesis highlighting

;; matching parenthesis highlighting settings
;; (setq show-paren-priority -50) ; without this matching parens aren't highlighted in region
;; (setq show-paren-delay 0)
;; (set-face-attribute 'show-paren-match nil :weight 'normal :foreground "lemon chiffon" :background "default")

;; default programs
(setq python-shell-interpreter "/usr/bin/python" ;; tell emacs where my python interpreter is
      ;;gnuplot-program "/usr/bin/gnuplot-qt"    ;; Fedora gnuplot location
      )

;; update path
(setq exec-path (append exec-path '("/opt/cuda/bin")))
(setenv "PATH" (concat
                "/opt/cuda/bin:"
                ;;"/opt/intel/composer_xe_2013_sp1.3.174/bin/intel64:"
                (getenv "PATH") ))
(setenv "LD_LIBRARY_PATH" (concat
                           "/opt/cuda/lib64:"
                           ;;"/opt/intel/composer_xe_2013_sp1.3.174/compiler/lib/intel64/:"
                           (getenv "LD_LIBRARY_PATH") ))

;; fortran settings
(setq-default f90-do-indent           2
	      f90-if-indent           2
	      f90-type-indent         2
	      f90-program-indent      2
              f90-continuation-indent 4)

;; line length settings
(setq-default fill-column 101)
(setq-default fci-rule-color "dim gray")
(add-hook 'f90-mode-hook        'fci-mode)
(add-hook 'c-mode-hook          'fci-mode)
(add-hook 'c++-mode-hook        'fci-mode)
(add-hook 'python-mode-hook     'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)

;; smooth scrolling
(setq scroll-step                1
      scroll-conservatively      10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1)) )

;; mode settings
;; is there a way to make C-e go to the actual end of the line in visual-line-mode??
(add-hook 'LaTeX-mode-hook 'flyspell-mode)    ;; auto spell-checking in latex
;;(add-hook 'LaTeX-mode-hook 'visual-line-mode) ;; break lines between words

(add-hook 'org-mode-hook 'flyspell-mode)             ;; auto spell-checking in org
(add-hook 'org-mode-hook 'visual-line-mode)          ;; break lines between words
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode) ;; wraped headers are indented properly
(setq org-pretty-entities t)                         ;; render math by default

;; set file ending defaults
(setq auto-mode-alist (cons '("\\.\\(F90\\|fpp\\)$" . f90-mode   ) auto-mode-alist)
      auto-mode-alist (cons '("\\.\\(cu\\|cl\\)$"   . c-mode     ) auto-mode-alist)
      auto-mode-alist (cons '(".m$"                 . octave-mode) auto-mode-alist) )

;; email
(setq-default user-mail-address "threeofsix@gmail.com")

;; keybindings
;; note: can put these all into a minor mode to group them together and deactivate easily, as well as
;;       easily override major mode settings. Follow directions here:
;;       http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;       for now, just manually override major mode settings that don't respect these
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f1>") 'create-shell)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
;; (global-set-key (kbd "M-X") 'in-directory)
;; (global-set-key "\C-ci"  'irc)
(add-hook 'f90-mode-hook (lambda () (local-set-key (kbd "C-c C-c") 'compile-in-dir)))

;; instead of minor mode, manually overriding for now
(add-hook 'ibuffer-mode-hook (lambda () (local-unset-key (kbd "M-o"))))


(provide 'init-config)
