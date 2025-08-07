;; Prevent package.el from initializing packages too early
;(setq package-enable-at-startup nil)

;; disable some UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set fonts
(pcase system-type
  ('gnu/linux
   (progn (set-frame-font "DejaVu Sans Mono 10")
          (set-face-attribute 'default (selected-frame) :height 105)))
  ('darwin
   (set-frame-font "Menlo 14")))
