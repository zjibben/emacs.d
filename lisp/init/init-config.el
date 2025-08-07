;; init-config
;;
;; basic emacs configuration

(setq-default display-time-default-load-average   nil     ; hide load average in modeline
              display-time-day-and-date           1       ; display both date and time
              indent-tabs-mode                    nil     ; spaces instead of tabs
              require-final-newline               t       ; ensure files end with a newline
              calc-angle-mode                     'rad    ; calc to radians-mode
              calc-multiplication-has-precedence  nil     ; sensible order of operations
              Info-fontify-maximum-menu-size      1000000 ; increase Info highlight limit
              shift-select-mode                   nil     ; shift/caps won't select text
              save-interprogram-paste-before-kill t       ; don't lose clipboard entries

              completions-format                  'vertical ; sort along columns rather than rows
              show-paren-delay                    0       ; show matching parentheses immediately
              proced-auto-update-flag             t
              proced-auto-update-interval         2
              doc-view-resolution                 300
              enable-remote-dir-locals            t
              compile-command "make -j8 "
              ediff-split-window-function #'split-window-horizontally

              comint-process-echoes t ; so that shell doesn't repeat every command back to me
              comint-terminfo-terminal "ansi"
              shell-command-switch "-ic" ; use interactive shell so bashrc is loaded

              ;; smooth scrolling
              scroll-step                1
              scroll-conservatively      10000
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              )

(display-time-mode 1) ; activate modeline time and date
(show-paren-mode 1) ; activate matching parenthesis highlighting
(defalias 'yes-or-no-p #'y-or-n-p) ; I don't like typing 2 or 3 characters when I can type 1

;; open shells in current window
(add-to-list 'display-buffer-alist '("^\\*shell\\*$" . (display-buffer-same-window)))

;; ensure environment is consistent with login environment
;; see http://stackoverflow.com/a/6415812
(when (or (daemonp) (display-graphic-p))
  (let ((path-from-shell (full-shell-command-to-clean-string "echo $PATH")))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; make tramp respect remote PATH variable
(with-eval-after-load "tramp" (add-to-list 'tramp-remote-path #'tramp-own-remote-path))

;; fortran settings
(setq-default f90-do-indent           2
              f90-if-indent           2
              f90-type-indent         2
              f90-program-indent      2
              f90-continuation-indent 4
              js-indent-level         2
              )
(add-to-list 'completion-ignored-extensions ".mod")

;; python settings
(when (eq system-type 'darwin)
  (setq-default python-shell-virtualenv-root
                (concat "/Users/" (user-login-name) "/python-venv/main")))
(setq-default python-indent-guess-indent-offset nil
              python-shell-interpreter "ipython3"
              python-shell-completion-native-enable nil
              python-shell-interpreter-args "--no-confirm-exit --simple-prompt")

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

;; mode settings
(add-to-mode-hooks '(prog-mode rst-mode markdown-mode)
                   (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil t)))
(add-to-mode-hooks '(prog-mode rst-mode markdown-mode)
                   (lambda () (add-hook 'before-save-hook #'untabify nil t)))
(add-hook 'rst-mode-hook #'flyspell-mode)

(use-package org
  :config
  (setq-mode-default 'org-mode fill-column 80)
  (add-hook 'org-mode-hook #'flyspell-mode)             ; auto spell-checking in org
  (add-hook 'org-mode-hook #'visual-line-mode)          ; break lines between words
  (add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode) ; wraped headers are indented properly
  (setq org-pretty-entities t)                         ; render math by default
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ; emphasize up to 20 lines instead of 1
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (defun create-org-log ()
    "Add an org log timestamp at point."
    (interactive)
    (org-insert-drawer nil "LOGBOOK")
    (insert "- ")
    (org-insert-time-stamp (current-time) nil t)
    (insert " Created")
    (org-up-element)
    (org-up-element)
    (org-cycle)
    (next-line)
    (org-return-indent)
    (org-open-line 1)))

;; set file ending defaults
(add-all-to-list 'auto-mode-alist
                 '("\\.\\(F90\\|fpp\\)$" . f90-mode)
                 '(".cu$" . c++-mode)
                 '(".h$" . c++-mode)
                 '(".cl$" . c-mode)
                 '(".m$" . octave-mode)
                 '(".pdf$" . pdf-view-mode))

;; keybindings
;; note: can put these all into a minor mode to group them together and deactivate easily,
;;       as well as easily override major mode settings. Follow directions here:
;;       http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;       for now, just manually override major mode settings that don't respect these
(global-set-key (kbd "C-x C-k") #'kill-current-buffer)
(global-set-key (kbd "C-x M-e") #'apply-macro-to-region-lines)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "M-o") #'next-multiframe-window)
(global-set-key (kbd "M-O") #'previous-multiframe-window)
(global-set-key (kbd "M-Y") (lambda () (interactive) (yank-pop -1)))
(global-set-key (kbd "<f1>") #'create-shell)
(global-set-key (kbd "<f2>") #'create-python-shell)

;; could put these mode-specific types of shortcuts into a minor mode as well
(modes-set-key '(f90-mode c-mode c++-mode) (kbd "C-c C-c") #'compile-in-dir)
(mode-unset-key 'ibuffer-mode-hook (kbd "M-o")) ; should put in minor mode instead of override

(provide 'init-config)
