(server-start)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package) ;; Load package manager
(setq package-enable-at-startup nil)   ; To prevent initialising twice

;; Add the Marmalade repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Get use-package moving
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))
(use-package async
  :ensure t)
(use-package js2-mode
  :ensure t)
(use-package js2-refactor
  :ensure t)
(use-package tern
  :ensure t)
(use-package paredit
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package graphviz-dot-mode
  :ensure t)
(use-package aggressive-indent
  :ensure t)
(use-package yasnippet
  :ensure t)
(use-package common-lisp-snippets
  :ensure t
  :config
  (require 'common-lisp-snippets))
(use-package gpastel
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package ob-clojurescript
  :ensure t)
;; A nice sidebar package
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :commands (all-the-icons-dired-mode)))
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  )
(use-package xelb
  :ensure t
  )
(use-package exwm
  :ensure t
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Allow resizing of certain windows that were being dicks
  (setq window-divider-default-right-width 1)
  (window-divider-mode)
  )
(use-package desktop-environment
  :ensure t
  :config
  (require 'desktop-environment)
  (desktop-environment-mode t))
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))
(use-package tide
  :init
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(use-package cider
  :ensure t
  )
(use-package slime
  :ensure t
  :config
  (add-to-list 'slime-contribs 'slime-repl)
  )
(use-package org
  :pin org
  :ensure t
  :init
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("revtex4-1"
                   "\\documentclass{revtex4-1}"
                   ("\\section{%s}" . "\\section*{%s}"))))
  (add-hook 'iESS-mode-hook
            (local-set-key (kbd "[?\\t]") 'complete-at-point)
            )
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-image-actual-width 600)
  (org-babel-do-load-languages
   'org-babel-load-languages (quote ((emacs-lisp . t)
                                     (sqlite . t)
                                     (clojure . t)
                                     (clojurescript . t)
                                     (sagemath . t)
                                     (calc . t)
                                     (lisp . t)
                                     (python . t))))
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq org-babel-python-command "python3")
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (require 'cider)


  :config
  (global-set-key [f10] 'org-capture)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; inline most image types in org-mode via the 'convert' utility
  (with-eval-after-load "org"
    (setq image-file-name-extensions
          (quote
           ("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp")))

    (setq org-image-actual-width 600)

    (setq org-imagemagick-display-command "convert -density 600 \"%s\" -thumbnail \"%sx%s>\" \"%s\"")

    (defun org-display-inline-images (&optional include-linked refresh beg end)
      "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This
can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
      (interactive "P")
      (unless refresh
        (org-remove-inline-images)
        (if (fboundp 'clear-image-cache) (clear-image-cache)))
      (save-excursion
        (save-restriction
          (widen)
          (setq beg (or beg (point-min)) end (or end (point-max)))
          (goto-char beg)
          (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                            (substring (org-image-file-name-regexp) 0 -2)
                            "\\)\\]" (if include-linked "" "\\]")))
                old file ov img)
            (while (re-search-forward re end t)
              (setq old (get-char-property-and-overlay (match-beginning 1)
                                                       'org-image-overlay)
                    file (expand-file-name
                          (concat (or (match-string 3) "") (match-string 4))))
              (when (file-exists-p file)
                (let ((file-thumb (format "%s%s_thumb.png" (file-name-directory file) (file-name-base file))))
                  (if (file-exists-p file-thumb)
                      (let ((thumb-time (nth 5 (file-attributes file-thumb 'string)))
                            (file-time (nth 5 (file-attributes file 'string))))
                        (if (time-less-p thumb-time file-time)
                            (shell-command (format org-imagemagick-display-command
                                                   file org-image-actual-width org-image-actual-width file-thumb) nil nil)))
                    (shell-command (format org-imagemagick-display-command
                                           file org-image-actual-width org-image-actual-width file-thumb) nil nil))
                  (if (and (car-safe old) refresh)
                      (image-refresh (overlay-get (cdr old) 'display))
                    (setq img (save-match-data (create-image file-thumb)))
                    (when img
                      (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                      (overlay-put ov 'display img)
                      (overlay-put ov 'face 'default)
                      (overlay-put ov 'org-image-overlay t)
                      (overlay-put ov 'modification-hooks
                                   (list 'org-display-inline-remove-overlay))
                      (push ov org-inline-image-overlays)))))))))))
  )
(use-package ob-sagemath
  :ensure t
  :after org
  :init
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))

  ;; C-c c for asynchronous evaluating (only for SageMath code blocks).
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

  ;; Do not confirm before evaluation
  (setq org-confirm-babel-evaluate nil)

  ;; Do not evaluate code blocks when exporting.
  (setq org-export-babel-evaluate nil)

  ;; Show images when opening a file.
  (setq org-startup-with-inline-images t)

  ;; Show images after evaluating code blocks.
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (setenv "SAGE_ROOT" "/home/rblack/.miniconda3/envs/sage")
  (setenv "SAGE_LOCAL" "/home/rblack/.miniconda3/envs/sage")
  )
;; (use-package org-ref
;;   :ensure t
;;   :after org
;;   :init
;;   (setq org-ref-completion-library 'org-ref-ivy-cite)
;;   (setq org-ref-default-bibliography '("~/projects/misc/bibliography/globalrefs.bib")
;;         org-ref-pdf-directory "~/projects/misc/bibliography/bibtex-pdfs/"
;;         org-ref-bibliography-notes "~/projects/misc/bibliography/notes.org")
;;   (setq org-latex-pdf-process
;;         '("pdflatex -interaction nonstopmode -output-directory %o %f"
;; 	      "bibtex %b"
;; 	      "pdflatex -interaction nonstopmode -output-directory %o %f"
;; 	      "pdflatex -interaction nonstopmode -output-directory %o %f"))
;;   )
(use-package ox-pandoc
  :ensure t
  :after org
  )
(use-package org-download
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook
            '(lambda ()
               (if (buffer-file-name)
                   (setq org-download-image-dir
                         (concat (file-name-sans-extension (buffer-file-name)) "-org-images"))
                 (setq org-download-image-dir "org-images")))))
(use-package notmuch
  :ensure t
  :init
  (setq message-kill-buffer-on-exit t)
  )


(use-package tex
  :ensure auctex
  :config
  (use-package auctex-latexmk :ensure t)

  (require 'auctex-latexmk)
  (auctex-latexmk-setup)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)

  (require 'tex-fold-linebreaks)
  (add-hook 'LaTeX-mode-hook 'tex-fold-mode)
  (add-hook 'LaTeX-mode-hook 'tex-fold-linebreaks-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook 'flycheck-mode)
  (setq ispell-program-name "aspell")
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)

  (setq-default fill-column 95)
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))

  ;; Use variable width font faces in current buffer
  (defun my-buffer-face-mode-variable ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "TeX Gyre Bonum" :height 140 :width normal))
    (buffer-face-mode))

  (set-default 'preview-scale-function 1.2)
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        ;; TeX-source-correlate-start-server t
        ) ;; not sure if last line is neccessary

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            'TeX-revert-document-buffer)

  (add-hook 'TeX-mode-hook '(lambda ()
                              (company-mode)
                              (make-local-variable 'company-backends)
                              (company-auctex-init)
                              (tex-source-correlate-mode t)
                              (tex-pdf-mode t)
                              (set-fill-column 99999)
                              (my-buffer-face-mode-variable))))

(use-package leuven-theme
  :ensure t
  :config
  (set-cursor-color "#6666ff")
  :defer t)
(use-package zenburn-theme
  :ensure t
  :defer t)
(use-package github-theme
  :ensure t
  :defer t)
(use-package spacemacs-theme
  :ensure t
  :defer t)
(use-package monokai-theme
  :ensure t
  :defer t)
(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  ;; (helm-autoresize-mode t)
  (setq helm-M-x-fuzzy-match t)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (helm-mode 1)
  )
(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'helm-swoop)
  )
(use-package helm-ag
  :ensure t)
(use-package helm-ls-git
  :ensure t
  :config
  (global-set-key (kbd "C-c p f") 'helm-ls-git-ls)
  )
(use-package helm-themes
  :ensure t
  )
(use-package yaml-mode
  :ensure t)
(use-package smartparens
  :ensure t)
(use-package request-deferred
  :ensure t)
(use-package realgud
  :ensure t
  :defer t)
(use-package pos-tip
  :ensure t)
(use-package names
  :ensure t)
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
(use-package magit
  :ensure t
  :config
  (global-magit-file-mode t)
  (setq magit-diff-use-overlays nil)
  (setq magit-revert-buffers t))
(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode t)
  )
(use-package list-utils
  :ensure t)
(use-package kv
  :ensure t)
(use-package json-mode
  :ensure t)
(setq byte-compile-warnings nil)
(use-package python-mode
  :ensure t)
(use-package ipython
  :ensure t
  :after python-mode)
(setq byte-compile-warnings t)
(use-package flycheck
  :ensure t)
(use-package exec-path-from-shell
  :ensure t)
(use-package elm-mode
  :ensure t
  :config
  (add-hook 'elm-mode-hook '(lambda ()
                            (smartparens-mode)
                            (company-mode)
                            (make-local-variable 'company-backends))))

(use-package company
  :ensure t
  :config
  (use-package company-cmake :ensure t)
  (use-package company-c-headers :ensure t)
  (use-package company-auctex :ensure t)

  (define-key company-mode-map (kbd "M-TAB") 'company-complete-common)
  (define-key company-active-map (kbd "M-TAB") 'company-complete-common)

  (setq company-idle-delay            0.2
      company-minimum-prefix-length   2
      company-show-numbers            t
      company-tooltip-limit           20
      company-dabbrev-downcase        nil
      )
  )




(use-package cmake-mode
  :ensure t)
(use-package buffer-move
  :ensure t)
;; (use-package anaphora
;;   :ensure t)
(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities '("Erlangen"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US,en;")))

;; Various settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode)
(setq-default cursor-type 'box)
(defun no-bell ())
(setq ring-bell-function 'no-bell)
(setq visible-bell nil)
(setq ns-pop-up-frames 'nil)

;; Leftover keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x T") '(lambda () (interactive) (eshell 't)))
(global-set-key [f9] 'toggle-window-dedicated)
(global-set-key (kbd "C-c p f") 'helm-ls-git-ls)
(global-set-key (kbd "s-p") nil) ; no printing
(global-set-key (kbd "C-z") nil) ; no background
(global-set-key (kbd "s-w") nil) ; no closing
(global-set-key (kbd "M-z") 'zap-up-to-char) 

;; Let me kill buffers and downcase shit
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defun indent-buffer ()
  "Indent each nonblank line in the buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun indent-buffer-and-delete-trailing-whitespace ()
  "Remove all trailing whitespace in the current buffer and indent all nonblank lines."
  (interactive)
  (delete-trailing-whitespace (point-min) (point-max))
  (indent-region (point-min) (point-max) nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" "c72a772c104710300103307264c00a04210c00f6cc419a79b8af7890478f380e" "d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "807a7f4c2d0d331fc1798e6d38b890ce3582096b8d622ba3b491b2aa4345e962" "bf64dd3657eef02b3b5f7439d452c7b18f4b5c1e717e6037c8f2b61b9b3dbcf8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "efb148b9a120f417464713fe6cad47eb708dc45c7f2dbfeea4a7ec329214e63e" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(ein:jupyter-default-notebook-directory "~/projects")
 '(ein:slice-image t)
 '(ess-language "R" t)
 '(fci-rule-color "#969896")
 '(notmuch-fcc-dirs (quote (("robert.blackwell@fau.de" . "fau/Sent"))))
 '(notmuch-poll-script "notmuch-poll.sh")
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(org-agenda-files
   (quote
    ("~/projects/manuscripts/motorspaper/plots.org" "~/projects/manuscripts/motorspaper/molmot_rb/plots.org")))
 '(package-selected-packages
   (quote
    (gpastel common-lisp-snippets aggressive-indent graphviz-dot-mode helm-themes paredit rainbow-delimiters cider helm-swoop swiper helm-company helm-ag helm-ls-git yaml-mode yasnippet esh-autosuggest desktop-environment exwm xelb ob-sagemath ox-pandoc htmlize slime ob-clojurescript magit-todos magit-todo tide web-mode typescript-mode notmuch pdf-tools company-tern js2-refactor xref-js2 smartparens glsl-mode evil lsp-ui company-lsp cquery lsp-mode auctex-latexmk ein anaconda-mode markdown-mode fortpy imenu-anywhere github-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow org light-soap-theme monokai-theme sunny-day-theme spacemacs-theme zenburn-theme magit google-this leuven-theme wttrin use-package org-download multiple-cursors dired-sidebar auctex)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(preview-default-document-pt 12)
 '(request-backend (quote url-retrieve))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp-auth.fau.de")
 '(smtpmail-smtp-service 25)
 '(tab-width 4)
 '(tex-fold-linebreaks-rebind-characters nil)
 '(tex-fold-linebreaks-sentence-end-punctuation (quote (("." . ".") ("?" . "?") ("!" . "!"))))
 '(tool-bar-mode nil)
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-annotate-background "#b0cde7")
 '(vc-annotate-color-map
   (quote
    ((20 . "#969896")
     (40 . "#183691")
     (60 . "#969896")
     (80 . "#969896")
     (100 . "#969896")
     (120 . "#a71d5d")
     (140 . "#969896")
     (160 . "#969896")
     (180 . "#969896")
     (200 . "#969896")
     (220 . "#63a35c")
     (240 . "#0086b3")
     (260 . "#795da3")
     (280 . "#969896")
     (300 . "#0086b3")
     (320 . "#969896")
     (340 . "#a71d5d")
     (360 . "#969896"))))
 '(vc-annotate-very-old-color "#969896"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Input Mono" :foundry "nil" :slant normal :weight medium :height 100 :width narrow))))
 '(preview-reference-face ((t nil))))

;; mac specific shit
(when (equal system-type 'darwin)
  ;; requires coreutils ('brew install coreutils', silly)
  (if (file-exists-p "/usr/local/bin/gls")
      (setq insert-directory-program "/usr/local/bin/gls"))
  (setq ns-use-native-fullscreen nil)
  (defun run-iterm ()
    (interactive)
    (shell-command (concat "open -a /Applications/iTerm.app .")))
  (global-set-key (kbd "s-w") 'nil)
  (global-set-key (kbd "s-q") 'nil)
  )


;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))


(show-paren-mode 1)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(global-subword-mode t)

(add-hook 'c-mode-common-hook '(lambda ()
                                 (company-mode)
                                 (electric-indent-mode)
                                 (smartparens-mode)
                                 (yas-minor-mode)
                                 (setq c-eldoc-cpp-command "/usr/bin/clang")
                                 (eldoc-mode)
                                 (setq cquery-extra-init-params
                                       '(:completion (:detailedLabel t)))
                                 (lsp-cquery-enable)
                                 ))


(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'c++-mode-hook '(lambda ()
                            (setq cquery-extra-init-params
                                  '(:completion (:detailedLabel t)))
                            (lsp-cquery-enable)
                            (setq company-transformers nil
                                  company-lsp-async t
                                  company-lsp-cache-candidates nil)
                            (company-mode)
                            (make-local-variable 'company-backends)
                            (setq company-backends '((company-lsp)))
                            (flycheck-mode)
                            ))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (make-local-variable 'lisp-indent-function)
             (setq lisp-indent-function 'common-lisp-indent-function)
             (paredit-mode t)
             (aggressive-indent-mode t)
             (rainbow-delimiters-mode)
             (make-variable-buffer-local 'tab-always-indent)
             (setq tab-always-indent 'complete)
             (yas-minor-mode t)
             ))

(add-hook 'python-mode-hook '(lambda ()
                               ;; (elpy-mode)
                               (anaconda-mode)
                               (yas-minor-mode)
                               (smartparens-mode)
                               (company-mode)
                               ))

;; Standard fortpy.el setting
(require 'fortran-tags)
(add-hook 'f90-mode-hook '(lambda ()
                            (fortpy-setup)
                            (fortran-tags-mode)
                            ))
(setq fortpy-complete-on-percent t)
(setq fortpy-complete-on-bracket t)
(setq f90-if-indent 2)
(setq f90-do-indent 2)
(setq f90-type-indent 2)


;; (xterm-mouse-mode t)
;; (setq x-select-enable-clipboard t)
;; (setq mouse-sel-mode t)
;; (setq mouse-autoselect-window t)

;; Transparency
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 100)

;; I don't want to be warned about discarding undo info.
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)
(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))
(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))
(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))
(add-hook 'after-save-hook #'gtags-update-hook)

;; Mode line setup
(column-number-mode t)
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize " %4l: " face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "  ")))
   " "

   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n "
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "

   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "  "
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
                    ;; :foreground "gray60" :background "gray20"
                    :inverse-video nil
                    :box '(:line-width 2 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    ;; :foreground "gray80" :background "gray40"
                    :inverse-video nil
                    :box '(:line-width 2 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    ;; :foreground "#c82829" :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    ;; :foreground "gray60"
                    )
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    ;; :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Input Mono" :height 80)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    ;; :foreground "gray80"
                    )
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    ;; :foreground "gray40"
                    :height 110)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    ;; :foreground "#718c00"
                    )
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    ;; :foreground "black" :background "#eab700"
                    )


(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


(auto-insert-mode)
 ;; *NOTE* Trailing slash important
(setq auto-insert-directory (concat (getenv "HOME") "/projects/misc/templates/"))
(setq auto-insert-query nil)
(define-auto-insert "\\.tex$" "latex-template.tex")
(define-auto-insert "\\Makefile$" "Makefile-template")


(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))
;; (ns-do-applescript "tell application \"Emacs\" to ")

(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)


(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


(setq calendar-latitude 49.5897)
(setq calendar-longitude 11.0120)
(setq calendar-location-name "Erlangen, Bayern")

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/ess")
;; (require 'ess-site)

(require 'tw2-mode)
(defun tw2-toggle-dev ()
  "Toggle twee2 dev server"
  (interactive)
  (let ((default-directory (locate-dominating-file "." "package.json")))
    (if (get-process "twee2-dev")
        (interrupt-process "twee2-dev")
      (start-process "twee2-dev" "*twee2*" "npm" "run" "dev"))))
(setq tw2-imenu-generic-expression
      '(("Passage" "^::*\\(.*\\)" 1)))
(defun tw2-open ()
  (interactive)
  (let ((default-directory (locate-dominating-file "." "package.json")))
    (if (get-process "twee2-dev")
        (shell-command "npm run open")
      (tw2-toggle-dev))))

(add-hook 'tw2-mode-hook
          '(lambda ()
             (local-set-key (kbd "TAB") 'self-insert-command)
             (local-set-key (kbd "C-c w") 'tw2-toggle-dev)
             (local-set-key (kbd "C-c o") 'tw2-open)
             ;; (local-set-key (kbd "C-c i") 'ivy-imenu-anywhere)
             (setq imenu-generic-expression tw2-imenu-generic-expression)
             (visual-line-mode t)
             (smartparens-mode)
             ))

(put 'narrow-to-region 'disabled nil)

(require 'recentf)
(add-to-list 'recentf-exclude ".*personal.*")

(setq ein:use-auto-complete t)

;; js2-mode stuff
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
(require 'tern)
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(load-theme 'tsdh-dark)
