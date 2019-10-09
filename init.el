(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

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
(use-package multi-term
  :ensure t
  :config
  (add-to-list 'term-bind-key-alist (cons "M-1" 'term-send-raw-meta))
  (add-to-list 'term-bind-key-alist (cons "M-2" 'term-send-raw-meta))
  (add-to-list 'term-bind-key-alist (cons "M-3" 'term-send-raw-meta))
  (add-to-list 'term-bind-key-alist (cons "M-DEL" 'term-send-raw-meta))
  )
(use-package paradox
  :ensure t)
(use-package password-store
  :ensure t)
(use-package pinentry
  :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
(use-package password-store
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package ob-async
  :ensure t
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  )
;; (use-package jupyter
;;   :ensure nil)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp)
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "~/.emacs.d/ccls.sh")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  )
(use-package clang-format+
  :ensure t)
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-ui-sideline-show-hover 'nil)
  (setq lsp-prefer-flymake 'nil)
  )
(use-package elpy
  :ensure t
  :config
  (setq python-shell-interpreter "python"
        ;; python-shell-interpreter-args "-i --simple-prompt"
        ;; python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic --simple-prompt"
        ;; python-shell-prompt-regexp ">>> "
        ;; python-shell-prompt-output-regexp ""
        ;; python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        ;; python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        ;; python-shell-completion-string-code "';'.join(get_ipython().Completer.all_comnpletions('''%s'''))\n"
        )
  (defun my-python-before-save-hook ()
    (when (eq major-mode 'python-mode)
      (elpy-black-fix-code)))

  (add-hook 'before-save-hook #'my-python-before-save-hook)

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map)))

  (elpy-enable)
  )
(use-package highlight-indentation
  :ensure t)
(use-package s
  :ensure t)
(use-package pyvenv
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
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
  :config)
(use-package all-the-icons-dired
  ;; M-x all-the-icons-install-fonts
  :commands (all-the-icons-dired-mode))
(use-package fish-completion
  :ensure t
  :config
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode)))
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  :config
  (if (eq system-type 'darwin)
      (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
    (custom-set-variables
     '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
    )
  (menu-bar-mode -1))
(use-package xelb
  :disabled
  )
(use-package exwm
  :disabled
  :config
  (if (equal system-name "Pilgrim")
      (progn
        (require 'exwm-randr)
        (setq exwm-randr-workspace-output-plist '(0 "DP-2" 1 "DP-0"))
        ;; (add-hook 'exwm-randr-screen-change-hook
        ;;           (lambda ()
        ;;             (start-process-shell-command
        ;;              "xrandr" nil "xrandr --output DP-0 --rotate normal --right-of DP-2 --auto --rotate left")))
        (exwm-randr-enable))
    )
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (setq exwm-workspace-number 2)

  ;; Allow resizing of certain windows that were being dicks
  (setq window-divider-default-right-width 1)
  (window-divider-mode))
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
(use-package company-tern
  :init
  :ensure t)
(use-package cider
  :ensure t
  )
(use-package slime
  :ensure t
  :config
  (add-to-list 'slime-contribs 'slime-repl)
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
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
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-image-actual-width 600)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (sqlite . t)
                               (clojure . t)
                               (clojurescript . t)
                               (calc . t)
                               (lisp . t)
                               (shell . t)
                               (python . t)
                               ;; (jupyter . t)
                               ))
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq org-babel-python-command "python3")
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (require 'cider)


  :config
  (global-set-key [f10] 'org-capture)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/projects/agenda/agenda.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/projects/agenda/journal.org")
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
    (setq buffer-face-mode-face '(:family "TeX Gyre Bonum" :height 90 :width normal))
    (buffer-face-mode))

  (set-default 'preview-scale-function 1.2)
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        )

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

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))
(use-package color-theme-modern
  :ensure t)
(use-package leuven-theme
  :ensure t
  :config
  (set-cursor-color "#6666ff")
  :defer t)
(use-package cyberpunk-theme
  :ensure t
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
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

  ;; Use fuzzy finding instead of clumsy reverse search
  (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))
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
(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities '("Erlangen"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US,en;")))
(use-package ein
  :ensure t)
(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

;; Various settings
(tool-bar-mode -1)
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))
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
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "3bc187cd480ad79f151b593f7cb7d4ad869b19741247589238c353f637e7fb21" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "36bab4e2aa8165f538e6d223ee1d2a0ef918ccba09e18c62cf8594467685a3b6" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" "c72a772c104710300103307264c00a04210c00f6cc419a79b8af7890478f380e" "d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "807a7f4c2d0d331fc1798e6d38b890ce3582096b8d622ba3b491b2aa4345e962" "bf64dd3657eef02b3b5f7439d452c7b18f4b5c1e717e6037c8f2b61b9b3dbcf8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "efb148b9a120f417464713fe6cad47eb708dc45c7f2dbfeea4a7ec329214e63e" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(eshell-buffer-maximum-lines 8092)
 '(eshell-history-size 4096)
 '(eshell-output-filter-functions
   (quote
    (eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-handle-ansi-color eshell-watch-for-password-prompt)))
 '(eshell-visual-commands
   (quote
    ("ipython" "gnuplot" "alsamixer" "htop" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm")))
 '(fci-rule-color "#383838")
 '(gdb-many-windows t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(helm-ls-git-default-sources
   (quote
    (helm-source-ls-git-buffers helm-source-ls-git helm-source-ls-git-status)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(iwconfig-program "/sbin/iw")
 '(large-file-warning-threshold nil)
 '(notmuch-fcc-dirs (quote (("robert.blackwell@fau.de" . "fau/Sent"))))
 '(notmuch-poll-script "notmuch-poll.sh")
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(org-agenda-custom-commands
   (quote
    (("h" "Daily habits"
      ((agenda "" nil))
      ((org-agenda-show-log t)
       (org-agenda-ndays 7)
       (org-agenda-log-mode-items
        (quote
         (state)))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notregexp)
          ":DAILY:"))))))))
 '(org-agenda-files
   (quote
    ("~/projects/agenda/agenda.org" "~/projects/agenda/recurring.org" "~/projects/agenda/journal.org")))
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (clang-format+ edit-server ein fish-completion ccls paradox multi-term color-theme-modern cyberpunk-theme password-store rainbow-mode git-gutter-fringe groovy-mode pinentry ob-async all-the-icons-dired gpastel common-lisp-snippets aggressive-indent graphviz-dot-mode helm-themes paredit rainbow-delimiters cider helm-swoop swiper helm-company helm-ag helm-ls-git yaml-mode yasnippet esh-autosuggest desktop-environment exwm ob-sagemath ox-pandoc htmlize ob-clojurescript magit-todos magit-todo typescript-mode notmuch pdf-tools company-tern js2-refactor xref-js2 glsl-mode evil lsp-ui company-lsp cquery lsp-mode auctex-latexmk markdown-mode fortpy imenu-anywhere github-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow light-soap-theme monokai-theme sunny-day-theme zenburn-theme magit google-this leuven-theme wttrin use-package org-download multiple-cursors dired-sidebar auctex)))
 '(paradox-github-token t)
 '(pdf-tools-handle-upgrades nil)
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(preview-default-document-pt 12)
 '(request-backend (quote url-retrieve))
 '(safe-local-variable-values
   (quote
    ((eval let nil
           (org-babel-goto-named-src-block "init_block")
           (org-babel-execute-src-block)))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(slurm-filter-user-at-start nil)
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

;; Set default font
(set-face-attribute 'default nil
                    :family "Input Mono"
                    :height 120
                    :weight 'normal
                    :width 'normal)

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
                                 (local-set-key (kbd "TAB") 'clang-format-region)
                                 (local-set-key (kbd "C-M-\\") 'clang-format-region)
                                 (eldoc-mode)
                                 (make-local-variable 'company-backends)
                                 (setq company-backends '((company-lsp)))
                                 ))


(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'c++-mode-hook '(lambda ()
                            (setq company-transformers nil
                                  company-lsp-async t
                                  company-lsp-cache-candidates nil)
                            (company-mode)
                            (make-local-variable 'company-backends)
                            (setq company-backends '((company-lsp)))
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
(add-to-list 'recentf-exclude ".*stuff.*")
(add-to-list 'recentf-exclude ".*elpa.*")

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

(defun node-start-dev ()
  "Toggle node dev server"
  (interactive)
  (let ((default-directory (locate-dominating-file "." "package.json")))
    (ansi-term (concat (getenv "HOME") "/.emacs.d/npm-dev.sh") "node-dev")
    ))

(add-to-list 'tramp-remote-path "~/.miniconda3/bin")

(let ((process-environment tramp-remote-process-environment))
  (setenv "ENV" "$HOME/.profile")
  (setq tramp-remote-process-environment process-environment))

(add-to-list 'tramp-remote-process-environment
             (format "DISPLAY=%s" (getenv "DISPLAY")))

(if (equal system-name "0xDEADBEEF")
    (display-battery-mode t))

(add-to-list 'emacs-lisp-mode-hook (lambda ()
                                     (paredit-mode t)
                                     (aggressive-indent-mode t)
                                     (rainbow-delimiters-mode)
                                     (make-variable-buffer-local 'tab-always-indent)
                                     (setq tab-always-indent 'complete)
                                     (yas-minor-mode t)))


(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))


(require 'renpy-mode)
(add-hook 'renpy-mode-hook 'rainbow-mode)

;; use eshell version of sudo for access caching
(require 'esh-module)
(setq eshell-prefer-lisp-functions 'nil)
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 120) ; for one hour (time in secs)
(setq eshell-destroy-buffer-when-process-dies t)

;; https://www.reddit.com/r/emacs/comments/8ptscw/better_term_setting_for_shelleshell/
;; Note that eshell doesn't inherit from comint so that comment is nonsense, but the term works
(add-hook 'eshell-mode-hook (lambda () (eshell/export "TERM=dumb-emacs-ansi")))

(winner-mode t)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)


(global-set-key "\C-ca" 'org-agenda)

;; clickable links in ansi-term!
(add-hook 'term-mode-hook 'goto-address-mode)

(defun my-agenda-commit ()
  (interactive)
  (progn
    (org-save-all-org-buffers)
    (message "Committing org files in ~/projects/agenda/")
    (shell-command "cd ~/projects/agenda/ && git commit -am 'autocommit'")
    (shell-command "cd ~/projects/agenda/ && git push & /usr/bin/env true"))
  )

(defun my-agenda-pull ()
  (interactive)
  (progn
    ;; (org-save-all-org-buffers)
    (message "Updating agenda at ~/projects/agenda/")
    (shell-command "cd ~/projects/agenda/ && git pull")
    )
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/lisp/slurm.el")
(require 'slurm-mode)
(require 'slurm-script-mode)
