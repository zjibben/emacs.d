;;
;; init-theme
;;
;; initialize theming
;;
;; zjibben <threeofsix@gmail.com> 12/2015
;; 

;;(setq color-theme-is-global nil) ;; only for color-theme package?

;; define a function to set the theme based on the mode (gui/console)
;; and load it every time we open a new frame
(defun set-theme (&optional frame)
  (if frame (select-frame frame))
  (if (window-system frame)
      (progn  ;; then
        (if (<= (display-pixel-width) 1366) (set-frame-font "Monospace 9"))
        (load-theme 'ample t)
        (powerline-default-theme) )
    (progn  ;; else
      (disable-theme 'ample) ;; in case it was active
      (powerline-vim-theme) ) )
  (display-splash-screen)
  ;; matching parenthesis highlighting settings
  (set-face-attribute 'show-paren-match nil
                      :weight     'normal
                      :foreground "lemon chiffon"
                      :background "default") )
(add-hook 'after-make-frame-functions 'set-theme) ;; used to have argument 1 at the end?
(set-theme) ;; run manually for non-server/client mode

;; set color for highlighting current line
;;(custom-set-faces '(hl-line ((t (:background "gray9")))))

(provide 'init-theme)
