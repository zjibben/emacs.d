;; install packages
;;
;; install emacs packages if they aren't already installed

;; list of packages to have installed
(setq package-selected-packages
      '(
        adaptive-wrap
        ample-theme
        arduino-mode
        auctex
        cmake-mode
        djvu
        eimp
        elpy
        ;;ein
        fill-column-indicator
        flx-ido
        gnuplot-mode
        haskell-mode
        julia-mode
        magit
        markdown-mode
        ;;multi-term
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
        wordnut
        yaml-mode
        ))

;; install all the above packages, updating the package archive if necessary
(if (>= (string-to-number emacs-version) 25)
    (progn (package-refresh-contents)
	   (package-install-selected-packages))

  (progn (package-refresh-contents)
         (dolist (package package-selected-packages)
           (unless (package-installed-p package)
             (package-install package)))))

(provide 'init-install-packages)
