;; init-packages
;;
;; initialize repositories and packages

;; add repos
;;(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; these packages need to be organized somewhere
(use-package adaptive-wrap :ensure t)
(use-package fill-column-indicator :ensure t)
(use-package flx-ido :ensure t)
;;(use-package multi-term :ensure t)
(use-package smex :ensure t)
(use-package wordnut :ensure t)
(use-package vterm :ensure t)
(use-package counsel :ensure t)
(use-package counsel-etags :ensure t)
(use-package flycheck :ensure t)
(use-package lsp-ivy :ensure t)
(use-package company :ensure t)

;; add new modes
(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package rust-mode :ensure t :defer t)
;(use-package arduino-mode :ensure t :defer t)
(use-package cmake-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package julia-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package typescript-mode :ensure t)
(use-package pkgbuild-mode :ensure t :defer t)
(use-package powershell :ensure t :defer t)
;(use-package gnuplot-mode :ensure t)
(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq-default plantuml-jar-path (pcase (system-distro)
                                    ("Arch" "/opt/plantuml/plantuml.jar")
                                    ("Fedora" "/usr/share/java/plantuml.jar"))))
;; latex, pdfs, images, and djvu
(use-package djvu :ensure t :defer t)

(use-package auctex
  :ensure t
  :defer t
  :config
  (setq         TeX-auto-save   t
                TeX-parse-self  t)
  (setq-default TeX-master      nil
                TeX-engine     'xetex))

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install t nil t))

(use-package eimp
  :ensure t
  :defer t
  :config
  (add-hook 'image-mode-hook  'eimp-mode)
  ;;(add-hook 'eimp-mode-hook  'eimp-fit-image-to-window)
  )



(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-file-dispatch))

(use-package pinentry ; for gpg
  :ensure t
  :config
  (pinentry-start))

;; python
;;(use-package ein :ensure t)

(use-package python-info :ensure t :defer t)

(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable))

;; IDE & autocompletion stuff
(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (setq-default lsp-enable-semantic-highlighting nil))

(use-package clang-format
  :ensure t
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (fset 'c-indent-region 'clang-format-region)
              (fset 'c-indent-line-or-region 'clang-format-for-tab)
              (c-set-offset 'innamespace [0]))))

(use-package yasnippet :ensure t)
(use-package yasnippet-snippets :ensure t)

;; AI stuff
;;(use-package ellama :ensure t)

(use-package gptel
  :ensure t
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
  :ensure t
  :defer t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "OPENROUTER_API_KEY" (openrouter-api-key))
  :custom
  ;; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openrouter/deepseek/deepseek-r1-0528:free"))

(provide 'init-packages)
