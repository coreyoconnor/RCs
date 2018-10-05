
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst dot-emacs (concat (getenv "HOME") "/" "Development/" "RCs/" "real-emacs.el")
    "real dot emacs file")

(require 'bytecomp)
(setq compiled-dot-emacs (byte-compile-dest-file dot-emacs))

(if (or (not (file-exists-p compiled-dot-emacs))
	(file-newer-than-file-p dot-emacs compiled-dot-emacs)
        (equal (nth 4 (file-attributes dot-emacs)) (list 0 0)))
    (load dot-emacs)
  (load compiled-dot-emacs))

(add-hook 'kill-emacs-hook
          '(lambda () (and (file-newer-than-file-p dot-emacs compiled-dot-emacs)
                           (byte-compile-file dot-emacs))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   (vector "#373b41" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(background-color nil)
 '(background-mode dark)
 '(company-idle-delay nil)
 '(coq-prog-args
   (quote
    ("-I" "/home/corey/Development/cpdt_coconnor/cpdt/src")))
 '(cursor-color nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ecb-auto-activate t)
 '(ecb-display-default-dir-after-start t)
 '(ecb-fix-window-size (quote width))
 '(ecb-layout-name "dironly")
 '(ecb-layout-window-sizes
   (quote
    (("basic"
      (ecb-directories-buffer-name 0.25728155339805825 . 0.48214285714285715)
      (ecb-analyse-buffer-name 0.25728155339805825 . 0.5))
     ("left8"
      (ecb-directories-buffer-name 0.1796116504854369 . 0.26785714285714285)
      (ecb-sources-buffer-name 0.1796116504854369 . 0.25)
      (ecb-methods-buffer-name 0.1796116504854369 . 0.2857142857142857)
      (ecb-history-buffer-name 0.1796116504854369 . 0.17857142857142858)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 40)
 '(ensime-company-idle-delay 0)
 '(ensime-startup-notification nil)
 '(ensime-typecheck-idle-interval 4)
 '(ensime-typecheck-interval 4)
 '(foreground-color nil)
 '(inhibit-startup-screen t)
 '(nav-width 40)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   (quote
    (glsl-mode evil-tabs moody nerdtab go-mode evil-collection gdscript-mode js2-highlight-vars js2-mode evil-magit string-inflection yasnippet yard-mode yaml-mode window-numbering web-mode swiper smex scala-mode sbt-mode rubocop robe projectile-rails paredit omniref nix-mode nav mediawiki markdown-mode ido-ubiquitous idle-highlight-mode hydra hindent haskell-mode haml-mode groovy-mode gradle-mode fringe-helper flymake-ruby find-file-in-project evil enh-ruby-mode elscreen ecb dirtree company color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized auto-complete)))
 '(safe-local-variable-values
   (quote
    ((haskell-indentation-where-post-offset . 4)
     (haskell-indentation-where-pre-offset . 4)
     (haskell-indentation-ifte-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-layout-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
