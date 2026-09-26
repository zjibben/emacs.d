;; init-packages
;;
;; initialize repositories and packages

;; these packages need to be organized somewhere
(use-package adaptive-wrap)
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
  :demand t
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
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq-mode-default 'markdown-mode fill-column 80)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil t)))
  (add-hook 'markdown-mode-hook (lambda () (add-hook 'before-save-hook
                                                     (lambda () (untabify (point-min) (point-max)))
                                                     nil t)))
  (setq-default markdown-enable-math t
                markdown-command "pandoc"
                markdown-split-window-direction 'right)
  )
(use-package mermaid-mode)
(use-package texfrag
  :hook (markdown-mode . texfrag-mode))
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
  :mode ("\\.pdf\\'" . pdf-view-mode)
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
    (setq-default python-shell-virtualenv-root (expand-file-name "~/python-venv/main")))
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

(use-package eglot
  :hook (c++-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-ignored-server-capabilities :semanticTokensProvider))

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
  (setq-default gptel-include-reasoning "*GPT Reasoning*"
                ;;gptel-model   'deepseek/deepseek-r1-0528:free
                gptel-model   'anthropic.claude-sonnet-4-5-20250929-v1:0
                gptel-backend (gptel-make-openai "OpenRouter"
                                :host "openrouter.ai"
                                :endpoint "/api/v1/chat/completions"
                                :stream t
                                :key (openrouter-api-key)
                                :models '(deepseek/deepseek-r1-0528:free
                                          openai/gpt-oss-120b
                                          google/gemini-2.5-flash
                                          openai/o4-mini-high
                                          anthropic/claude-sonnet-4
                                          ))
                gptel-backend (gptel-make-openai "LANL AI Portal"
                                :host "aiportal-api.aws.lanl.gov"
                                :endpoint "/v1/chat/completions"
                                :stream t
                                :key (lanl-ai-portal-api-key)
                                :models '(anthropic.claude-sonnet-4-5-20250929-v1:0
                                          anthropic.claude-3-haiku-20240307-v1:0
                                          gpt-oss-120b
                                          ))
                ))

;; (use-package gptel-fn-complete
;;   :ensure t
;;   :config
;;   (defgroup gptel-context nil
;;     "Context helpers for gptel."
;;     :group 'gptel)

;;   (defcustom gptel-context-lines-around 10
;;     "Number of lines above and below point to add as context."
;;     :type 'integer
;;     :group 'gptel-context)

;;   (defun gptel-context-add-lines-around ()
;;     "Add N lines above and below point to GPTel context."
;;     (interactive)
;;     (gptel-context-remove-all)
;;     (let* ((n gptel-context-lines-around)
;;            (start
;;             (save-excursion
;;               (forward-line (- n))
;;               (point)))
;;            (end

;;               (forward-line n)
;;               (point))))
;;       (gptel-context--add-region (current-buffer) start end t)
;;       (message "Added lines %d..%d to GPTel context" start end)))

;;   (defadvice gptel-fn-complete (before add-context activate)
;;     "Add context before calling gptel-fn-complete."
;;     (gptel-context-add-lines-around))

;;   (advice-add
;;    'gptel--rewrite-accept
;;    :after
;;    (lambda (&rest _args)
;;      (symex-select-nearest-in-line)
;;      (gptel-context-remove-all)))

;;   (define-key lisp-mode-map (kbd "TAB") #'gptel-fn-complete)
;;   (define-key emacs-lisp-mode-map (kbd "TAB") #'gptel-fn-complete)
;;   )

;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   (setenv "OPENROUTER_API_KEY" (openrouter-api-key))
;;   :custom
;;   ;; See the Configuration section below
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-default-model "openrouter/deepseek/deepseek-r1-0528:free"))

;; /Users/zjibben/.local/share/uv/tools/aider-chat/bin/python -m pip install boto3
;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   (setenv "OPENAI_API_BASE" "https://aiportal-api.aws.lanl.gov")
;;   (setenv "OPENAI_API_KEY" (lanl-ai-portal-api-key))
;;   (setenv "AWS_ACCESS_KEY_ID" (lanl-ai-portal-api-key))
;;   (setenv "AWS_SECRET_ACCESS_KEY" (lanl-ai-portal-api-key))
;;   (setenv "REQUESTS_CA_BUNDLE" "/Library/Application Support/Mozilla/Certificates/LANLWINOLT-RootCA.pem")
;;   (setenv "SSL_CERT_FILE" "/Library/Application Support/Mozilla/Certificates/LANLWINOLT-RootCA.pem")
;;   (setq aidermacs-openai-api-key (lanl-ai-portal-api-key)
;;         aidermacs-openai-api-base "https://aiportal-api.aws.lanl.gov"
;;         aidermacs-model "openai/anthropic.claude-sonnet-4-5-20250929-v1:0")
;;   :custom
;;   ;; See the Configuration section below
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-default-model "openai/anthropic.claude-sonnet-4-5-20250929-v1:0"))

;; (use-package minuet
;;   :ensure t
;;   :config
;;   (setq minuet-provider 'openai-compatible)
;;   (setq minuet-request-timeout 2.5)
;;   (setq minuet-auto-suggestion-throttle-delay 1.5) ; Increase to reduce costs and avoid rate limits
;;   (setq minuet-auto-suggestion-debounce-delay 0.6) ; Increase to reduce costs and avoid rate limits

;;   (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
;;   (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
;;   (plist-put minuet-openai-compatible-options :model "deepseek/deepseek-r1-0528:free")

;;   ;; Prioritize throughput for faster completion
;;   (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
;;   (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
;;   (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9))

;; (use-package copilot
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook 'copilot-mode)
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;;   (setopt copilot-lsp-settings '(:github-enterprise (:uri "https://example2.ghe.com")))
;;   (add-to-list 'copilot-major-mode-alist '("f90" . "fortran"
;;                                            "cc" . "cpp")))

(provide 'init-packages)
