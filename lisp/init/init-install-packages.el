;; install packages
;;
;; install emacs packages if they aren't already installed

;; list of packages to have installed
(setq package-selected-packages
      '(
        adaptive-wrap
        arduino-mode
        auctex
        cmake-mode
        djvu
        dockerfile-mode
        eimp
        ;;ein
        elpy
        fill-column-indicator
        flx-ido
        ;;gnuplot-mode
        haskell-mode
        julia-mode
        lua-mode
        magit
        markdown-mode
        ;;multi-term
        pdf-tools
        pinentry
        pkgbuild-mode
        plantuml-mode
        powershell
        python-info
        rust-mode
        smex
        use-package
        wordnut
        yaml-mode
        vterm

        counsel
        counsel-etags
        lsp-mode
        clang-format
        flycheck
        lsp-ivy
        company
        ))

;; install all the above packages, updating the package archive if necessary
(package-refresh-contents)
(package-install-selected-packages t)

(provide 'init-install-packages)
