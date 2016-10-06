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
(setq package-selected-packages '(
                     adaptive-wrap
                     ample-theme
                     arduino-mode
                     auctex
                     cmake-mode
                     djvu
                     eimp
                     elpy
                     ;ein
                     fill-column-indicator
                     flx-ido
                     gnuplot-mode
                     haskell-mode
                     ;helm
                     julia-mode
                     latex-pretty-symbols
                     ;latex-preview-pane
                     magit
                     markdown-mode
                     ;multi-term
                     ;org-bullets
                     pdf-tools
                     pinentry
                     pkgbuild-mode
                     plantuml-mode
                     powerline
                     powershell
                     python-info
                     rust-mode
                     smex
                     use-package
                     wordnut))

;; install all the above packages, updating the package archive if necessary
(if (>= (string-to-number emacs-version) 25)
    (package-install-selected-packages)
    (progn
      (unless package-archive-contents (package-refresh-contents))
      (dolist (package package-selected-packages) (package-install package))))

;; TODO: patch latex-pretty-symbols

;; other packages worth noting:
;;   achievements
;;   color-theme
;;   tuareg
;;   multi-term
;;   helm
;;   ivy
;;   latex-preview-pane
;;   org-bullets

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
