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

;; define a function to set the theme based on the mode (gui/console)
;; and load it every time we open a new frame
(defun set-theme (&optional frame)
  (if frame (select-frame frame))
  (load-theme 'ample t)
  (if (window-system frame)
      (progn
        (set-frame-font "DejaVu Sans Mono 10")
        (set-face-attribute 'default (selected-frame) :height 105)
        (powerline-default-theme))
    (progn
      ;;(disable-theme 'ample) ;; in case it was active
      (set-face-background 'default "unspecified-bg" (selected-frame)) ;; terminal sets bg color
      (powerline-vim-theme) ) )
  (display-splash-screen)

  ;; fix issue where italics showing up as underlined
  (set-face-attribute 'italic nil
                      :slant 'italic
                      :underline nil)

  ;; matching parenthesis highlighting settings
  (set-face-attribute 'show-paren-match nil
                      :weight     'normal
                      :foreground "lemon chiffon"
                      :background "default"))


(add-hook 'after-make-frame-functions 'set-theme t) ;; append
(set-theme) ;; run manually for non-server/client mode

;; set color for highlighting current line
;;(custom-set-faces '(hl-line ((t (:background "gray9")))))

(provide 'init-theme)
