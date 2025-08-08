;; init-packages
;;
;; initialize repositories and packages

;; add repos
;;(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; these packages need to be organized somewhere
(use-package adaptive-wrap)
(use-package fill-column-indicator
  :demand t
  :config
  (add-to-mode-hooks '(f90-mode c-mode c++-mode python-mode emacs-lisp-mode sh-mode html-mode)
                     #'fci-mode)
  (setq-default fci-rule-color "dim gray"
                fill-column 100
                sentence-end-double-space nil)
  (setq-mode-default 'rst-mode fill-column 80))
;;(use-package multi-term)

;; (use-package flx-ido
;;   :init
;;   (setq-default ido-everywhere                      t
;;                 ido-enable-flex-matching            t
;;                 ido-ignore-extensions               t     ; ido ignores extensions like '~' and '.o'
;;                 ido-use-faces                       nil     ; use flx highlights
;;                 ido-default-buffer-method           'selected-window)
;;   :config
;;   (ido-mode 1)
;;   (flx-ido-mode 1))
;; (use-package smex
;;   :bind (("C-c C-c M-x" . execute-extended-command)
;;          ("C-s" . isearch-forward-regexp)
;;          ("C-r" . isearch-backward-regexp)
;;          ("C-M-s" . isearch-forward)
;;          ("C-M-r" . isearch-backward))
;;   :config
;;   (smex-initialize))

(use-package wordnut)
(use-package vterm)
(use-package counsel
  :bind (;; Ivy-based interface to standard commands
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         ;; ("<f1> f" . counsel-describe-function)
         ;; ("<f1> v" . counsel-describe-variable)
         ;; ("<f1> l" . counsel-find-library)
         ;; ("<f2> i" . counsel-info-lookup-symbol)
         ;; ("<f2> u" . counsel-unicode-char)
         ;; ("<f2> j" . counsel-set-variable)
         ("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)

         ;; ;; Ivy-based interface to shell and system tools
         ;; ("C-c c" . counsel-compile)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c j" . counsel-git-grep)
         ;; ("C-c L" . counsel-git-log)
         ;; ("C-c k" . counsel-rg)
         ;; ("C-c m" . counsel-linux-app)
         ;; ("C-c n" . counsel-fzf)
         ;; ("C-x l" . counsel-locate)
         ;; ("C-c J" . counsel-file-jump)
         ;; ("C-S-o" . counsel-rhythmbox)
         ;; ("C-c w" . counsel-wmctrl)

         ;; ;; Ivy-resume and other commands
         ;; ("C-c C-r" . ivy-resume)
         ;; ("C-c b" . counsel-bookmark)
         ;; ("C-c d" . counsel-descbinds)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c o" . counsel-outline)
         ;; ("C-c t" . counsel-load-theme)
         ;; ("C-c F" . counsel-org-file)

         :map ivy-minibuffer-map
         ;; ido-style folder navigation
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done)
         )
  :config
  ;; TODO -- find file should ignore files ending in ~ or .o, and temporary files #...#
  (ivy-mode 1)
  (counsel-mode 1)
  (setq-default ivy-use-virtual-buffers t
                ivy-re-builders-alist '((swiper-isearch . ivy--regex)
                                        (t              . ivy--regex-fuzzy)
                                        )
                counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))
(use-package counsel-etags)
(use-package flycheck)
(use-package lsp-ivy)

;; add new modes
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package rust-mode)
;; (use-package arduino-mode
;;   :bind ("C-c C-c" . #'compile-in-dir))
(use-package cmake-mode)
(use-package haskell-mode)
(use-package julia-mode)
(use-package lua-mode)
(use-package markdown-mode
  :config
  (setq-mode-default 'markdown-mode fill-column 99999999999999)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (setq-default markdown-command "pandoc")
  )
(use-package typescript-mode)
(use-package pkgbuild-mode)
(use-package powershell)
;;(use-package gnuplot-mode)
(use-package plantuml-mode
  :config
  (setq-default plantuml-jar-path (pcase (system-distro)
                                    ("Arch" "/opt/plantuml/plantuml.jar")
                                    ("Fedora" "/usr/share/java/plantuml.jar"))))
;; latex, pdfs, images, and djvu
(use-package djvu)

(use-package auctex
  :config
  (setq         TeX-auto-save   t
                TeX-parse-self  t)
  (setq-default TeX-master      nil
                TeX-engine     'xetex)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)    ; auto spell-checking in latex
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (add-all-to-list 'prettify-symbols-alist
                                                         '("\\varphi" . ?φ)
                                                         '("\\phi" . ?ϕ)
                                                         '("\\right)" . ?\))
                                                         '("\\left(" . ?\())))
  ;; is there a way to make C-e go to the actual end of the line in visual-line-mode??
  ;;(add-hook 'LaTeX-mode-hook #'visual-line-mode) ; break lines between words
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  )

(use-package pdf-tools
  :config
  (pdf-tools-install t nil t)
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode))

(use-package eimp
  :hook ((image-mode . eimp-mode)
         ;;(eimp-mode . eimp-fit-image-to-window)
         )
  ;; :config
  ;;(add-hook 'eimp-mode-hook  #'eimp-fit-image-to-window)
  )

(use-package magit
  :bind ("C-c g" . magit-file-dispatch))

(use-package pinentry ; for gpg
  :demand t
  :init
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  :config
  (pinentry-start))

;; python
;;(use-package ein)

(use-package elpy
  :config
  (elpy-enable))

(use-package python
  :init
  (when (eq system-type 'darwin)
    (setq-default python-shell-virtualenv-root
                  (concat "/Users/" (user-login-name) "/python-venv/main")))
  (setq-default python-indent-guess-indent-offset nil
                python-shell-interpreter "ipython3"
                python-shell-completion-native-enable nil
                python-shell-interpreter-args "--no-confirm-exit --simple-prompt")

  (defun create-python-shell ()
    "Open a new Python shell buffer."
    (interactive)

    ;; for some reason, if the current buffer is a python shell, creating a new
    ;; shell screws up existing ones. switch to temp buffer before making the new one.
    (with-temp-buffer
      (let ((buffer-name (generate-new-buffer-name "*python*")))
        (pop-to-buffer (python-shell-make-comint
                        (python-shell-calculate-command)
                        buffer-name
                        t))
        ;; rename the buffer after the fact, because Emacs's internal commands for
        ;; making Python shells do their own manipulation on any buffer name you hand it
        (rename-buffer buffer-name))))
  :bind ("<f2>" . #'create-python-shell)
  :commands python-shell-make-comint)

;; IDE & autocompletion stuff
(use-package company)

(use-package lsp-mode
  :hook (c++-mode . lsp)
  :config
  (setq-default lsp-enable-semantic-highlighting nil))

(use-package clang-format
  :demand t
  :hook (c++-mode . (lambda ()
              (fset 'c-indent-region 'clang-format-region)
              (fset 'c-indent-line-or-region 'clang-format-for-tab)
              (c-set-offset 'innamespace [0])))
  :config
  (defun only-forward-to-indentation ()
    "Move back-to-indentation if the first whitespace is forward.
This is used to replicate the behavior of TAB. Ignores all
arguments."
    (interactive)
    (if (> (save-excursion (back-to-indentation) (current-column))
           (current-column))
        (back-to-indentation)))

  (defun clang-format-for-tab ()
    (interactive)
    (clang-format (point) (point))
    (only-forward-to-indentation)))

(use-package yasnippet)
(use-package yasnippet-snippets)

;; AI stuff
;;(use-package ellama)

(use-package gptel
  :config
  (setq gptel-model   'deepseek/deepseek-r1-0528:free
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (openrouter-api-key)
          :models '(deepseek/deepseek-r1-0528:free
                    openai/gpt-oss-120b
                    google/gemini-2.5-flash
                    openai/o4-mini-high
                    anthropic/claude-sonnet-4
                    ))))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "OPENROUTER_API_KEY" (openrouter-api-key))
  :custom
  ;; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openrouter/deepseek/deepseek-r1-0528:free"))

(provide 'init-packages)
