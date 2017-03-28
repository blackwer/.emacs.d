;; Load package manager
(require 'package)


; list the packages you want
(setq package-list '(leuven-theme google-this ivy-hydra
                                  ivy-bibtex swiper counsel-gtags counsel-projectile counsel ivy
                                  jedi haskell-mode ensime sbt-mode ein applescript-mode zone-nyan
                                  zenburn-theme yaml-mode xterm-keybinder web-mode undo-tree
                                  sunny-day-theme spotify spacemacs-theme smartparens
                                  selectric-mode sane-term request-deferred realgud pos-tip org
                                  nyan-mode names multiple-cursors monokai-theme matlab-mode magit
                                  list-utils light-soap-theme kv json-mode ipython function-args
                                  flycheck exec-path-from-shell elpy elm-yasnippets elm-mode
                                  cuda-mode company-jedi company-irony company-cmake
                                  company-c-headers company-auctex cmake-mode buffer-move
                                  anaphora))

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



;; Get rid of everything.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(defun no-bell ())
(setq ring-bell-function 'no-bell)
(setq visible-bell nil)
(setq-default cursor-type 'box)
(setq ns-pop-up-frames 'nil)


;; Just LaTeX things
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'tex-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'tex-fold-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)
(setq-default fill-column 100)
(setq TeX-PDF-mode t)
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat (getenv "HOME") "/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
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
                            (my-buffer-face-mode-variable))
          )

;; Let me kill buffers and downcase shit
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq backup-directory-alist `(("." . "~/.saves")))
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
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     (#("LaTeX" 0 1
        (idx 0))
      "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     (#("BibTeX" 0 1
        (idx 1))
      "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     (#("Biber" 0 1
        (idx 2))
      "biber %s" TeX-run-Biber nil t :help "Run Biber")
     (#("View" 0 1
        (idx 3))
      "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b" TeX-run-discard-or-function t t :help "Run Viewer")
     (#("Print" 0 1
        (idx 4))
      "%p" TeX-run-command t t :help "Print the file")
     (#("Queue" 0 1
        (idx 5))
      "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     (#("File" 0 1
        (idx 6))
      "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     (#("Index" 0 1
        (idx 7))
      "makeindex %s" TeX-run-command nil t :help "Create index file")
     (#("Check" 0 1
        (idx 8))
      "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     (#("Spell" 0 1
        (idx 9))
      "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     (#("Clean" 0 1
        (idx 10))
      "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     (#("Clean All" 0 1
        (idx 11))
      "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     (#("Other" 0 1
        (idx 12))
      "" TeX-run-command t t :help "Run an arbitrary command"))))
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
                   company-oddmuse company-files company-dabbrev)))
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "807a7f4c2d0d331fc1798e6d38b890ce3582096b8d622ba3b491b2aa4345e962" "bf64dd3657eef02b3b5f7439d452c7b18f4b5c1e717e6037c8f2b61b9b3dbcf8" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "efb148b9a120f417464713fe6cad47eb708dc45c7f2dbfeea4a7ec329214e63e" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(doc-view-resolution 600)
 '(fci-rule-color "#efefef" t)
 '(magit-revert-buffers t t)
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(package-selected-packages
   (quote
    (org-plus-contrib gnuplot gnuplot-mode leuven-theme google-this ivy-hydra ivy-bibtex swiper counsel-gtags counsel-projectile counsel ivy jedi haskell-mode ensime sbt-mode ein applescript-mode zone-nyan zenburn-theme yaml-mode xterm-keybinder web-mode undo-tree sunny-day-theme spotify spacemacs-theme smartparens selectric-mode sane-term request-deferred realgud pos-tip org nyan-mode names multiple-cursors monokai-theme matlab-mode magit list-utils light-soap-theme kv json-mode ipython function-args flycheck exec-path-from-shell elpy elm-yasnippets elm-mode cuda-mode company-jedi company-irony company-cmake company-c-headers company-auctex cmake-mode buffer-move anaphora)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(projectile-switch-project-action (quote projectile-vc))
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
 '(vc-annotate-very-old-color "#DC8CC3"))
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

;; mac set keys and things
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta) ;; Bind meta to ALT
  (setq mac-command-modifier 'super) ;; Bind apple/command to  super if you want
  (setq mac-function-modifier 'hyper)
  (setq ns-use-native-fullscreen nil)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  )

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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

(require 'sane-term)
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)

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

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(projectile-global-mode)

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

(global-set-key [f7] 'spotify-previous)
(global-set-key [f8] 'spotify-next)
(global-set-key [f9] 'toggle-window-dedicated)
(global-set-key [f12] 'toggle-fullscreen)

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
                                        ; emacsclient [default -- keep?]
                                        ;   mode-line-client
   " "
                                        ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "  ")))
   " "
                                        ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
                                        ; narrow [default -- keep?]
   " %n "
                                        ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   ;; (:eval (propertize (format-mode-line minor-mode-alist)
   ;;                    'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "  "
                                        ; nyan-mode uses nyan cat as an alternative to %p
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

(defun my-insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(global-set-key "\C-c\C-i" 'my-insert-file-name)


;; ;; Stupid stupid things
;; (nyan-mode 't)

;;(benchmark-init/deactivate)


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
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-image-actual-width 400)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
