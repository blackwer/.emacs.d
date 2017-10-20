;; Load package manager
(require 'package)


; list the packages you want
(setq package-list '(leuven-theme google-this ivy-hydra
                     ivy-bibtex swiper counsel-gtags counsel ivy
                     jedi ensime sbt-mode ein applescript-mode
                     zone-nyan zenburn-theme yaml-mode
                     xterm-keybinder web-mode undo-tree
                     sunny-day-theme spotify spacemacs-theme
                     smartparens selectric-mode request-deferred
                     realgud pos-tip org nyan-mode names
                     multiple-cursors monokai-theme matlab-mode
                     magit list-utils light-soap-theme kv
                     json-mode ipython function-args flycheck
                     exec-path-from-shell elpy elm-yasnippets
                     elm-mode cuda-mode company-jedi
                     company-irony company-cmake
                     company-c-headers company-auctex cmake-mode
                     buffer-move anaphora org org-download))

;; Add the Marmalade repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-unstable" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Get rid of everything.
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


;; Just LaTeX things
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'tex-fold-mode)
(add-hook 'LaTeX-mode-hook 'smartparens-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'smartparens-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(setq ispell-program-name "aspell")
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)
(when (equal system-type 'darwin)
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))
(setq-default fill-column 95)
(setq TeX-PDF-mode t)
(setenv "PATH" (concat (getenv "HOME") "/bin:" (getenv "PATH")))
(setenv "PATH" (concat (getenv "HOME") ".miniconda2/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(add-to-list 'exec-path (concat (getenv "HOME") ".miniconda2/bin"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))

;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "CMU Serif" :height 200 :width normal))
  (buffer-face-mode))

(add-hook 'TeX-mode-hook '(lambda ()
                            (company-mode)
                            (make-local-variable 'company-backends)
                            (company-auctex-init)
                            (my-buffer-face-mode-variable)))

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
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-gtags company-irony company-jedi company-css company-elm company-clang company-xcode company-cmake company-capf
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-files)))
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" "c72a772c104710300103307264c00a04210c00f6cc419a79b8af7890478f380e" "d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "807a7f4c2d0d331fc1798e6d38b890ce3582096b8d622ba3b491b2aa4345e962" "bf64dd3657eef02b3b5f7439d452c7b18f4b5c1e717e6037c8f2b61b9b3dbcf8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "efb148b9a120f417464713fe6cad47eb708dc45c7f2dbfeea4a7ec329214e63e" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(doc-view-resolution 600)
 '(fci-rule-color "#efefef" t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(magit-revert-buffers t t)
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(ns-use-native-fullscreen nil)
 '(package-selected-packages
   (quote
    (irony-eldoc markdown-mode fortpy counsel-spotify spotify org-download org-plus-contrib gnuplot gnuplot-mode leuven-theme google-this ivy-hydra ivy-bibtex swiper counsel-gtags counsel ivy jedi ensime sbt-mode ein applescript-mode zone-nyan zenburn-theme yaml-mode xterm-keybinder web-mode undo-tree sunny-day-theme spacemacs-theme smartparens selectric-mode request-deferred realgud pos-tip org nyan-mode names multiple-cursors monokai-theme matlab-mode magit list-utils light-soap-theme kv json-mode ipython function-args flycheck exec-path-from-shell elpy elm-yasnippets elm-mode cuda-mode company-jedi company-irony company-cmake company-c-headers company-auctex cmake-mode buffer-move anaphora)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(recentf-exclude (quote ("\\.emacs.d.*")))
 '(red "#ffffff")
 '(request-backend (quote url-retrieve))
 '(sane-term-shell-command "/usr/local/bin/bash")
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Anonymous Pro" :foundry "nil" :slant normal :weight normal :height 160 :width normal)))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; mac specific shit
(when (equal system-type 'darwin)
  ;; requires coreutils ('brew install coreutils', silly)
  (if (file-exists-p "/usr/local/bin/gls")
      (setq insert-directory-program "/usr/local/bin/gls"))
  (defun run-iterm ()
    (interactive)
    (shell-command (concat "open -a /Applications/iTerm.app ."))))


(require 'multiple-cursors)


(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))
(ad-activate 'pop-to-buffer)

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
                                 ))


(add-hook 'c++-mode-hook '(lambda ()
                            (company-mode)
                            (make-local-variable 'company-backends)
                            (setq company-backends '((company-irony)))
                            (irony-mode)
                            ))


(add-to-list 'auto-mode-alist '("\\.rpy\\'" . python-mode))
(add-hook 'python-mode-hook '(lambda ()
                               (jedi:setup)
                               (yas-minor-mode)
                               (smartparens-mode)
                               (company-mode)
                               ))

(add-hook 'elm-mode-hook '(lambda ()
                            (smartparens-mode)
                            (company-mode)
                            (make-local-variable 'company-backends)
                            ;; (setq company-backends '(company-elm))
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


(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "M-TAB") 'company-complete-common)
     (define-key company-active-map (kbd "M-TAB") 'company-complete-common)
     ))

(setq company-idle-delay              0.2
      company-minimum-prefix-length   2
      company-show-numbers            t
      company-tooltip-limit           20
      company-dabbrev-downcase        nil
      )

(set-default 'preview-scale-function 1.2)
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
   ;; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
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
                    :foreground "gray60" :background "gray20"
                    :inverse-video nil
                    :box '(:line-width 2 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray40"
                    :inverse-video nil
                    :box '(:line-width 2 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray40"
                    :height 110)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")


;; ===== Scrolling fix for advanced mouses =============================
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-8] 'down-slightly)
(global-set-key [mouse-9] 'up-slightly)


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

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c b") 'ivy-bibtex)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x T") '(lambda () (interactive) (eshell 't)))
(add-hook 'eshell-mode-hook 'auto-image-file-mode)
(global-set-key [f6] 'google-this-search)
(global-set-key [f7] 'spotify-previous)
(global-set-key [f8] 'spotify-next)
(global-set-key [f9] 'toggle-window-dedicated)
(global-set-key [f10] 'org-capture)
(global-set-key [f12] 'toggle-fullscreen)
(global-set-key (kbd "C-c p f") 'counsel-git)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "s-p") nil)

(ivy-add-actions t
                 '(("I" (lambda (x) (with-ivy-window (insert x))) "insert")))
(ivy-add-actions 'counsel-find-file
                 '(("F" (lambda (x) (with-ivy-window (insert (file-relative-name x))))
                    "insert relative file name")
                   ("B" (lambda (x)
                          (with-ivy-window
                            (insert (file-name-nondirectory (replace-regexp-in-string "/\\'" "" x)))))
                    "insert file name without any directory information")))
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(eval-after-load "swiper"
  '(progn
     (define-key swiper-map (kbd "C-.")
       (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
     (define-key swiper-map (kbd "M-.")
       (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))
     ))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-image-actual-width 600)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("revtex4-1"
                 "\\documentclass{revtex4-1}"
                 ("\\section{%s}" . "\\section*{%s}"))))

(require 'org-download)
(add-hook 'org-mode-hook
          '(lambda ()
             (if (buffer-file-name) 
               (setq org-download-image-dir
                     (concat (file-name-sans-extension (buffer-file-name)) "-org-images"))
               (setq org-download-image-dir "org-images")
               )
             )
          )

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


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


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq calendar-latitude 49.5897)
(setq calendar-longitude 11.0120)
(setq calendar-location-name "Erlangen, Bayern")

