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
              js-indent-level         4
              )
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

;; mode settings
(add-hook 'rst-mode-hook #'flyspell-mode)
(add-to-mode-hooks '(prog-mode rst-mode)
                   (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil t)))
(add-to-mode-hooks '(prog-mode rst-mode)
                   (lambda () (add-hook 'before-save-hook
                                        (lambda () (untabify (point-min) (point-max)))
                                        nil t)))

(add-to-mode-hooks '(f90-mode c-mode c++-mode python-mode emacs-lisp-mode sh-mode html-mode)
                   #'display-fill-column-indicator-mode)
(setq-default fill-column 100
              sentence-end-double-space nil)
(setq-mode-default 'rst-mode fill-column 80)

;; org-mode
(use-package org-ql)
(use-package org-super-agenda
  :config
  (org-super-agenda-mode 1))
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq-default org-directory    "~/sync/docs/org"
                org-agenda-files '("inbox.org" "personal.org" "work.org" "media.org")
                org-refile-targets '((org-agenda-files . t))
                org-outline-path-complete-in-steps nil
                org-refile-use-outline-path t
                org-pretty-entities  t ; render math by default
                org-log-done 'time
                org-log-into-drawer t
                org-clock-in-switch-to-state "DOING"
                org-priority-highest 1
                org-priority-lowest 5
                org-priority-default 3
                ;org-deadline-warning-days 7

                org-agenda-custom-commands
                '(("w" "Work Agenda"
                   ((org-ql-block '(and (path "work.org")
                                        (todo)
                                        (or (tags "active")
                                            (priority >= "2")
                                            (todo "DOING")
                                            (deadline auto)
                                            (scheduled :to today)))
                                  ((org-ql-block-header "Group 1")
                                   (org-super-agenda-groups '((:auto-parent t)))
                                   ))
                    (org-ql-block '(and (path "work.org")
                                        (todo)
                                        (not (todo "DOING"))
                                        (not (tags "active"))
                                        (priority < "2"))
                                  ((org-ql-block-header "Unassigned")))
                    ))
                  ("pa" "Personal Agenda"
                   ((agenda "")
                    (tags-todo "work")
                    (tags "office")))
                  ("pm" "Media"
                   ((org-ql-block '(and (path "media.org")
                                        (or (todo "DOING")
                                            (tags "active")))
                                  ((org-ql-block-header "Started")
                                   (org-super-agenda-groups '((:auto-parent t)))))
                    (org-ql-block '(and (path "media.org")
                                        (todo)
                                        (not (todo "DOING"))
                                        (or (priority >= "2")
                                            (tags "active")))
                                  ((org-ql-block-header "High Priority"))))))

                ;; note DOING is ahead of TODO for sorting purposes
                org-todo-keywords '((sequence "DOING(i!)" "TODO(t)"
                                              "|" "DONE(d!)" "CANCELED(x@)" "DELEGATED"))
                org-todo-keyword-faces '(("DOING" . "yellow"))
                org-capture-templates
                '(("t" "Todo" entry (file+headline "~/sync/docs/org/inbox.org" "Tasks")
                   "* TODO %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n\n%a\n%i")
                  ("w" "Todo without context" entry
                   (file+headline "~/sync/docs/org/inbox.org" "Tasks")
                   "* TODO %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n\n%i")
                  ("n" "Note" entry (file+headline "~/sync/docs/org/inbox.org" "Notes")
                   "* %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n\n%a\n%i")
                  ("j" "Journal" entry (file+olp+datetree "~/sync/docs/org/inbox.org" "Journal")
                   "* %u %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n\n%a\n%i"))
                )
  (setq-mode-default 'org-mode fill-column 80)
  (add-hook 'org-mode-hook #'flyspell-mode)             ; auto spell-checking in org
  (add-hook 'org-mode-hook #'visual-line-mode)          ; break lines between words
  (add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode) ; wraped headers are indented properly
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ; emphasize up to 20 lines instead of 1
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-componentsi)
  (defun create-org-log ()
    "Add an org log timestamp at point."
    (interactive)
    (org-insert-drawer nil "PROPERTIES")
    (insert ":CREATED: ")
    (org-insert-time-stamp (current-time) nil t)
    (org-up-element)
    (org-cycle)
    (next-line)
    (org-return-indent)
    (org-open-line 1)))
(use-package org-roam)

;; set file ending defaults
(add-all-to-list 'auto-mode-alist
                 '("\\.\\(F90\\|fpp\\)$" . f90-mode)
                 '(".cu$" . c++-mode)
                 '(".h$" . c++-mode)
                 '(".tpp$" . c++-mode)
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

;; could put these mode-specific types of shortcuts into a minor mode as well
(modes-set-key '(f90-mode c-mode c++-mode) (kbd "C-c C-c") #'compile-in-dir)
(mode-unset-key 'ibuffer-mode-hook (kbd "M-o")) ; should put in minor mode instead of override

(provide 'init-config)
