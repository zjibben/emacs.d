;;
;; emacs-lite configuration file
;;
;; I use this to load up a lighter emacs in the terminal for quick edits.
;; It essentially contains most of the configuration from init-config,
;; without setting up my packages and more complex commands and modes.
;;
;; zjibben <threeofsix@gmail.com>

;; basic configuration
(setq-default indent-tabs-mode                   nil     ; spaces instead of tabs
              calc-angle-mode                    'rad    ; calc to radians-mode
              calc-multiplication-has-precedence nil     ; sensible order of operations
              Info-fontify-maximum-menu-size     1000000 ; increase Info highlight limit
              shift-select-mode                  nil     ; shift/caps won't select text
              ido-everywhere                     t
              ido-enable-flex-matching           t
              ido-ignore-extensions              t       ; ido ignores extensions like '~' and '.o'
              completions-format                 'vertical ; sort along columns rather than rows
              show-paren-delay                   0       ; show matching parentheses immediately
              scroll-step                        1       ; smooth scrolling
              scroll-conservatively              10000
              )
;;(ido-mode           1) ; enable ido-mode for switching buffers and finding files (slows startup)
(menu-bar-mode     -1) ; deactivate menubar
(tool-bar-mode     -1) ; deactivate toolbar
(scroll-bar-mode   -1) ; deactivate scroll bar
(show-paren-mode    1) ; activate matching parenthesis highlighting
(defalias 'yes-or-no-p 'y-or-n-p)

;; fortran settings
(setq-default f90-do-indent           2
	      f90-if-indent           2
	      f90-type-indent         2
	      f90-program-indent      2
              f90-continuation-indent 4)

;; set file ending defaults
(add-to-list 'auto-mode-alist '("\\.\\(F90\\|fpp\\)$" . f90-mode   ))
(add-to-list 'auto-mode-alist '("\\.\\(cu\\|cl\\)$"   . c-mode     ))
(add-to-list 'auto-mode-alist '(".m$"                 . octave-mode))

;; keybindings
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
(add-hook 'ibuffer-mode-hook (lambda () (local-unset-key (kbd "M-o"))))
