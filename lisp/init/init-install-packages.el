;; install packages
;;
;; install emacs packages if they aren't already installed

;; list of packages to have installed
(setq package-selected-packages
      '(
        adaptive-wrap
        ample-theme
        auctex
        cmake-mode
        ;;elpy ;; requires >=emacs-24.4
        fill-column-indicator
        flx-ido
        gnuplot-mode
        haskell-mode
        julia-mode
        ;;magit ;; requires >=emacs-25.1
        ;;markdown-mode ;; requires >=emacs-24.4
        ;;multi-term
        powerline
        smex
        yaml-mode
        ))

;; install all the above packages, updating the package archive if necessary
(package-refresh-contents)
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-install-packages)
