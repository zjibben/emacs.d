;;
;; init-theme
;;
;; initialize theming
;;
;; zjibben <threeofsix@gmail.com>

;;(setq color-theme-is-global nil) ;; only for color-theme package?

;; define a function to set the theme based on the mode (gui/console)
;; and load it every time we open a new frame
(defun set-theme (&optional frame)
  (if frame (select-frame frame))
  (load-theme 'ample t)
  (if (window-system frame)
      (progn  ;; then
        (if (<= (display-pixel-width) 1366)
            (set-frame-font "Monospace 9")
          (set-frame-font "Monospace 11"))
        (powerline-default-theme) )
    (progn  ;; else
      ;;(disable-theme 'ample) ;; in case it was active
      (set-face-background 'default "unspecified-bg" (selected-frame)) ;; terminal sets bg color
      (powerline-vim-theme) ) )
  (display-splash-screen)
  ;; matching parenthesis highlighting settings
  (set-face-attribute 'show-paren-match nil
                      :weight     'normal
                      :foreground "lemon chiffon"
                      :background "default") )
(add-hook 'after-make-frame-functions 'set-theme t) ;; append
(set-theme) ;; run manually for non-server/client mode

;; set color for highlighting current line
;;(custom-set-faces '(hl-line ((t (:background "gray9")))))

(provide 'init-theme)
