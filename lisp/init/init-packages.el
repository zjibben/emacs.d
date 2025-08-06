;; init-packages
;;
;; initialize repositories and packages

;; add repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; install missing packages
(require 'init-install-packages)

(require 'dockerfile-mode)

;; configure installed packages
;; AUCTeX
(setq         TeX-auto-save   t
              TeX-parse-self  t)
(setq-default TeX-master      nil
              TeX-engine     'xetex)


(pdf-tools-install t nil t)

;; plantuml-mode
(setq-default plantuml-jar-path (pcase (system-distro)
                                  ("Arch" "/opt/plantuml/plantuml.jar")
                                  ("Fedora" "/usr/share/java/plantuml.jar")))

;; eimp (fit images to window by default)
(add-hook 'image-mode-hook  'eimp-mode)
;(add-hook 'eimp-mode-hook  'eimp-fit-image-to-window)

;; arduino-mode
(defun arduino-compile ()
  "Compile Arduino program and upload."
  (interactive)
  (compile "make upload")
  (pop-to-buffer "*compilation*"))

(pinentry-start) ;; gpg stuff
;;(elpy-enable)


;; magit
;; override the default
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;; mu4e
;; mu needs to be separately installed, so don't fail if mu4e isn't found
;; system/user-dependent material is initialized in init-private-info.el
(if (string-match ".*\.lanl\.gov" system-name)
    (add-to-list 'load-path "~/opt/mu/share/emacs/site-lisp/mu4e"))
(require 'mu4e nil t)
(require 'mu4e-contrib nil t)
(setq mu4e-html2text-command 'mu4e-shr2text
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval  600
      message-send-mail-function 'smtpmail-send-it)

;; AI stuff
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
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "OPENROUTER_API_KEY" (openrouter-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openrouter/deepseek/deepseek-r1-0528:free"))

(provide 'init-packages)
