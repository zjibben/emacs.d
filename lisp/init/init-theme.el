;; init-theme
;;
;; initialize theming

;;(setq color-theme-is-global nil) ;; only for color-theme package?

;; give emacs a dark window
;; (defun dark-window-border (&optional frame)
;;   (if frame (select-frame frame))
;;   (if (window-system frame)
;;       (shell-command "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' \
;;                             -id $(xprop -root | awk '/^_NET_ACTIVE_WINDOW/ {print $5}')")))
;; (add-hook 'after-make-frame-functions 'dark-window-border t)

(use-package ample-theme)
(use-package powerline)

(defun set-theme-for-frame (frame)
  "Define a function to set the theme based on the mode (gui/console) and
load it every time we open a new frame."
  (with-selected-frame frame
    (if (display-graphic-p frame)
        ;; GUI settings
        (progn
          (load-theme 'ample t)
          (powerline-default-theme)
          ;;(telephone-line-mode t)
          )

      ;; Terminal settings
      (progn
        ;;(disable-theme 'ample) ;; in case it was active
        (set-face-background 'default "unspecified-bg" (selected-frame)) ;; terminal sets bg color
        (powerline-vim-theme)))

    ;; fix issue where italics showing up as underlined
    (set-face-attribute 'italic frame
                        :slant 'italic
                        :underline nil)

    ;; matching parenthesis highlighting settings
    (set-face-attribute 'show-paren-match nil
                        :weight     'normal
                        :foreground "lemon chiffon"
                        :background "default")

    (display-splash-screen)))

(add-hook 'after-make-frame-functions #'raise-frame) ; automatically focus new frames
(add-hook 'after-make-frame-functions #'set-theme-for-frame)
;;(when (daemonp) (add-hook 'server-after-make-frame-hook #'set-theme-for-frame))
(unless (daemonp) (set-theme-for-frame (selected-frame)))

;; (use-package telephone-line
;;   :config
;;   (defface my-telephone-line-accent-active
;;     '((t (:foreground "#bdbdb3" :background "grey22" :inherit mode-line))) "")
;;   (defface my-telephone-line-accent-inactive
;;     '((t (:foreground "#bdbdb3" :background "grey11" :inherit mode-line-inactive))) "")
;;   (setq telephone-line-faces
;;         '((accent . (my-telephone-line-accent-active . my-telephone-line-accent-inactive))
;;           (nil . (mode-line . mode-line-inactive))))

;;   (setq telephone-line-lhs
;;         '((nil   . (telephone-line-buffer-segment
;;                     telephone-line-process-segment))
;;           (accent . (telephone-line-major-mode-segment))
;;           (nil    . (telephone-line-vc-segment
;;                                         ;telephone-line-erc-modified-channels-segment
;;                      ))))
;;   (setq telephone-line-rhs
;;         '((nil    . (telephone-line-misc-info-segment))
;;           (accent . (telephone-line-minor-mode-segment))
;;           (nil   . (telephone-line-airline-position-segment)))))

;; This is a workaround to a latency issue I was experiencing. Typing or moving
;; the cursor or changing windows would incur random latency, especially when
;; typing individual characters slowly, one at a time, and especially when
;; putting Emacs in fullscreen on X. Seemed to disappear on Wayland. Found
;; solutions suggested here:
;; https://www.reddit.com/r/emacs/comments/zgp6kw/gui_emacs_weird_the_bigger_the_frame_size_is_the/
;; https://github.com/doomemacs/doomemacs/issues/2217
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; set color for highlighting current line
;;(custom-set-faces '(hl-line ((t (:background "gray9")))))

(provide 'init-theme)
