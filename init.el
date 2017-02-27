;; set coding system so emacs doesn't choke on melpa file listings
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; packages loaded every session
(require 'cl)
(require 'cl-lib)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'esh-mode)

;; load the package manager
(require 'package) ;;

;;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
  ;;                       ("marmalade" . "https://marmalade-repo.org/packages/")
    ;;                     ("melpa" . "https://melpa.org/packages/")))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Make a list of the packages you want
(setq my-package-list '(;; gnu packages
                        auctex
                        windresize
                        diff-hl
                        adaptive-wrap
                        ;; melpa packages
                        command-log-mode
                        undo-tree
                        better-defaults
                        diminish
                        smart-mode-line
                        dired+
                        howdoi
                        auctex-latexmk
                        magit
                        mouse3
			sos
                        counsel
                        flx-ido
                        smex
                        ivy-bibtex
                        hydra
                        ivy-hydra
                        which-key
                        outline-magic
                        smooth-scroll
			sublimity ;;mimics sublime text
                        unfill
                        company
                        ess
                        markdown-mode
                        polymode
                        eval-in-repl
                        haskell-mode
                        ghc
                        company-ghci
                        flycheck
                        scala-mode
                        ensime
                        sbt-mode
                        exec-path-from-shell
                        htmlize
                        autopair
                        sdcv ;; stardictionary
                        osx-dictionary
                        define-word
                        ox-pandoc))

;; Activate package autoloads
(package-initialize)
(setq package-initialize nil)

;; make sure stale packages don't get loaded
(dolist (package my-package-list)
  (if (featurep package)
      (unload-feature package t)))
;; Install packages in package-list if they are not already installed
(unless (every #'package-installed-p my-package-list)
  (package-refresh-contents)
  (dolist (package my-package-list)
    (when (not (package-installed-p package))
      (package-install package))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(custom-safe-themes
   (quote
    ("ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages (quote (magit)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; add custom theme directory to path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") 

;; add themes 
(add-hook 'after-init-hook 'sml/setup)
(setq sml/theme 'dark)

(add-hook 'after-init-hook (lambda () (load-theme 'ample-zen)))

;; add custom lisp directory to path
(let ((default-directory (concat user-emacs-directory "lisp/")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; on OSX Emacs needs help setting up the system paths
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; better defaults are well, better... but we don't always agree
(menu-bar-mode 1)

;; Mouse scrolling behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode 1) ; makes the region visible
(line-number-mode 1)    ; makes the line number show up
(column-number-mode 1)  ; makes the column number show up

;; ;; smooth scrolling with C-up/C-down
(require 'smooth-scroll)
(smooth-scroll-mode)
(global-set-key [(control down)] 'scroll-up-1)
(global-set-key [(control up)] 'scroll-down-1)
(global-set-key [(control left)] 'scroll-right-1)
(global-set-key [(control right)] 'scroll-left-1)

;; make home and end behave
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; enable toggling paragraph un-fill
(define-key global-map "\M-Q" 'unfill-paragraph)

;;; line wrapping
;; neck beards be damned, we don't need to hard wrap. The editor can soft wrap for us.
;;(remove-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
;;(add-hook 'text-mode-hook 'visual-line-mode 1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.


;; don't require two spaces for sentence end.
(setq sentence-end-double-space nil)

;; The beeping can be annoying--turn it off
(set-variable 'visible-bell t)

;; save place -- move to the place I was last time I visited this file
(save-place-mode t)

;; Use CUA mode to make life easier. We _do_ use standard copy/paste etc. 
(cua-mode t)


;; sublimity mode - sublime text minimap and smooth scroll
(require 'sublimity)
;;(require 'sublimity-scroll)
(require 'sublimity-map)
;;(require 'sublimity-attractive)

(sublimity-mode 1)

;; pairs parathenses and quotations
(require 'autopair)
  (autopair-global-mode) ;; to enable in all buffers
