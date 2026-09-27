;; -*- lexical-binding: t; -*-
;;
;; Prevent package.el from initializing packages too early
;(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(width . 102))

(define-advice display-warning
    (:around (orig-fun type &rest args) silence-missing-lexical-binding-cookie)
  "Silence warnings about missing lexical-binding cookies."
  (if (memq 'missing-lexbind-cookie type)
      t
    (apply orig-fun type args)))
