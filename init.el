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
                        pandoc-mode
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

;; set PATH to shell path so that things like pandoc work correctly
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
 '(package-selected-packages
   (quote
    (zotxt polymode ess-view pandoc-mode pandoc writeroom-mode magit)))
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
;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(require 'sublimity-attractive)

;;(sublimity-mode 1)

;; pairs parathenses and quotations
(require 'autopair)
  (autopair-global-mode) ;; to enable in all buffers

;; ;; Make control-z undo
(global-undo-tree-mode t)
(global-set-key (kbd "C-z") 'undo)
(define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-x u") 'undo)
(define-key undo-tree-map (kbd "C-x U") 'undo-tree-visualize)
(define-key undo-tree-map (kbd "M-z") 'undo-tree-visualize)
;; Make C-g quit undo tree
(define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "<escape> <escape> <escape>") 'undo-tree-visualizer-quit)



;; ;; 
;; Make right-click do something close to what people expect
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)
;; (global-set-key (kbd "C-f") 'isearch-forward)
;; (global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-o") 'counsel-find-file)
(define-key cua-global-keymap (kbd "<C-S-SPC>") nil)
(define-key cua-global-keymap (kbd "<C-return>") nil)
(setq cua-rectangle-mark-key (kbd "<C-S-SPC>"))
(define-key cua-global-keymap (kbd "<C-S-SPC>") 'cua-rectangle-mark-mode)

;; Undo/redo window changes
(winner-mode 1)

;; use ace-window for navigating windows
(global-set-key (kbd "C-x O") 'ace-window)
(with-eval-after-load "ace-window"
  (set-face-attribute 'aw-leading-char-face nil :height 2.5))

;; enable on-the-fly spell checking
(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)))
;; prevent flyspell from finding mistakes in the code
(add-hook 'prog-mode-hook
          (lambda ()
            ;; `ispell-comments-and-strings'
            (flyspell-prog-mode)))

;; ispell should not check code blocks in org mode
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))

;; Dictionaries

;; default in case we don't find something local

(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c S-D") 'define-word)

;; use dictionary app on os x
(when (memq window-system '(mac ns))
  (global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
  (global-set-key (kbd "C-c S-D") 'osx-dictionary-search-input))


;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
;; Search files in directory with C-S
(global-set-key (kbd "C-S-s") 'find-grep-dired); default if we don't find something better
(cond
 ((executable-find "rg") ; search with ripgrep if we have it
  (global-set-key (kbd "C-S-s") 'counsel-rg))
 ((executable-find "ag") ; otherwise search with ag if we have it
  (global-set-key (kbd "C-S-s") 'counsel-ag))
 ((executable-find "pt") ; otherwise search with pt if we have it
  (global-set-key (kbd "C-S-s") 'counsel-pt)))
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-S-v") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-o") 'counsel-find-file)
;; search for files to open with "C-O=
(when (memq window-system '(mac ns)) ; use mdfind on Mac. TODO: what about windows?
  (setq locate-command "mdfind")
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))
(global-set-key (kbd "C-x C-S-F") 'find-name-dired) ; default in case we don't have something better
(global-set-key (kbd "C-x C-S-F") 'counsel-locate)
(global-set-key (kbd "C-S-O") 'counsel-locate)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "<C-tab>") 'counsel-company)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; Ivy-resume and other commands

(global-set-key (kbd "C-c i") 'ivy-resume)

;; Make Ivy more like ido
;;(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
;;(define-key ivy-minibuffer-map (kbd "C-d") 'ivy-done)
;;(define-key ivy-minibuffer-map (kbd "C-b") 'ivy-immediate-done)
;;(define-key ivy-minibuffer-map (kbd "C-f") 'ivy-immediate-done)

;; show recently opened files
(setq recentf-max-menu-items 50)
(recentf-mode 1)

(require 'company)
;; cancel if input doesn't match, be patient, and don't complete automatically.
(setq company-require-match nil
      company-async-timeout 6
      company-idle-delay nil)
;; complete using C-tab
(global-set-key (kbd "<C-tab>") 'counsel-company)
;; use C-n and C-p to cycle through completions
;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

(require 'company-capf)
;; put company-capf and company-files at the beginning of the list
(setq company-backends
      '(company-files company-capf company-nxml company-css company-cmake company-semantic company-clang company-xcode company-eclim))
(setq-default company-backends
              '(company-files company-capf company-nxml company-css company-cmake company-semantic company-clang company-xcode company-eclim))

;;Use tab to complete.
;; See https://github.com/company-mode/company-mode/issues/94 for another approach.

;; this is a copy-paste from the company-package with extra conditions to make
;; sure we don't offer completions in the middle of a word.

(defun my-company-indent-or-complete-common ()
  "Indent the current line or region, or complete the common part."
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((and (not (looking-at "\\w\\|\\s_"))
         (memq indent-line-function
               '(indent-relative indent-relative-maybe)))
    (company-complete-common))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (call-interactively #'indent-for-tab-command)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 (not (looking-at "\\w\\|\\s_")))
        (company-complete-common))))))

;;(define-key company-mode-map (kbd "<tab>") 'my-company-indent-or-complete-common)

;; not sure why this should be set in a hook, but that is how the manual says to do it.
(add-hook 'after-init-hook 'global-company-mode)

;; (require 'which-key)
(which-key-mode)

;; (require 'flycheck)
(global-flycheck-mode)

;;; Configure outline minor modes
;; Less crazy key bindings for outline-minor-mode
(setq outline-minor-mode-prefix "\C-c\C-o")
;; load outline-magic along with outline-minor-mode
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
            (define-key outline-minor-mode-map "\C-c\C-o\t" 'outline-cycle)))

;; require the main file containing common functions
(require 'eval-in-repl)
(setq comint-process-echoes t)

;; truncate lines in comint buffers
(add-hook 'comint-mode-hook
          (lambda()
            (setq truncate-lines 1)))

;; Scroll down for input and output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)


;; ESS settings
(require 'ess-site)

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . Rnw-mode))

;; Make TeX and RefTex aware of Snw and Rnw files
(setq reftex-file-extensions
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

;; Lets you do 'C-c C-c Sweave' from your Rnw file
(add-hook 'Rnw-mode-hook
	  (lambda ()
	    (add-to-list 'TeX-command-list
			 '("Sweave" "R CMD Sweave %s"
			   TeX-run-command nil (latex-mode) :help "Run Sweave") t)
	    (add-to-list 'TeX-command-list
			 '("LatexSweave" "%l %(mode) %s"
			   TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
	    (setq TeX-command-default "Sweave")))


;; set knitr to sweave documents
(setq ess-swv-processor "'knitr")

;; Make shift-enter do a lot in ess 
(setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1 nil t))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))
  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
	(call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key [(shift return)] 'my-ess-eval)))
  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key [C-up] 'comint-previous-input)
	       (local-set-key [C-down] 'comint-next-input)))
 (add-hook 'Rnw-mode-hook 
          '(lambda() 
             (local-set-key [(shift return)] 'my-ess-eval))) 
  (require 'ess-site)

;; uniquify names 
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)  

;;lintr and flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'ess-mode-hook
            (lambda () (flycheck-mode t)))


;; C-x o goes to the next window, Shift+direction arrow moves between frames.

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
;; replace disputed key in org-mode so framemove works 
(setq org-replace-disputed-keys t) 

;; pandoc setq
(setq pandoc-use-async nil)


;; require ess-view for viewing spreadsheets of dataframes
(require 'ess-view)

;; Polymode to allow editing and running chunks of R-markdown files
(require 'poly-R)
(require 'poly-markdown)
;;; polymode + markdown
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; polymode + R
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
