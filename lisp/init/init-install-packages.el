;;
;; install packages
;;
;; this script will install emacs packages I use when they are not already found
;; 
;; inspired by http://stackoverflow.com/q/10092322
;; note package-install checks if a package is already installed before installing
;;
;; zjibben <threeofsix@gmail.com>

;; list of packages to have installed
(setq package-install-list '(
                     adaptive-wrap
                     ample-theme
                     auctex
                     djvu
                     eimp
                     fill-column-indicator
                     gnuplot-mode
                     haskell-mode
                     ;helm
                     latex-pretty-symbols
                     ;latex-preview-pane
                     magit
                     ;multi-term
                     ;org-bullets
                     pdf-tools
                     pkgbuild-mode
                     plantuml-mode
                     powerline
                     python-info
                     rust-mode
                     smex
                     ;tuareg
                     ;use-package
                     ))

;; install all the above packages, updating the package archive if necessary
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-install-list) (package-install package))

;; TODO: patch latex-pretty-symbols

;; other packages worth noting:
;;   achievements
;;   color-theme

;; other themes:
;;   almost-monokai
;;   base16-theme
;;   boron-theme
;;   darcula-theme
;;   monokai-theme
;;   solarized-theme
;;   sublime-themes
;;   zenburn-theme

(provide 'init-install-packages)
