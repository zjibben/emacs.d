;; -*- lexical-binding: t; -*-
;;
;; Prevent package.el from initializing packages too early
;(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(width . 105))
(add-to-list 'default-frame-alist '(height . 60))

(define-advice display-warning
    (:around (orig-fun type &rest args) silence-missing-lexical-binding-cookie)
  "Silence warnings about missing lexical-binding cookies."
  (if (or (eq type 'missing-lexbind-cookie)
          (and (listp type) (memq 'missing-lexbind-cookie type)))
      t
    (apply orig-fun type args)))
