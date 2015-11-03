;;; init.el --- Initialization
;;; -*- coding: utf-8 -*-
;;; Commentary:
;;
;; Emacs configuration that aims to add lots of enabled-by-default,
;; non-intrusive useful features while keeping traditional Emacs
;; keybindings and workflow.
;;

;;; Code:


;; PACKAGES :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


(when (>= emacs-major-version 24)
  (setq package-list '(
					   ;; pymacs
                       ;; auto-complete
                       ;; fixmee
                       ;; rainbow-mode
                       ;; smartrep
                       ;; virtualenvwrapper
					   anaconda-mode
					   anchored-transpose
					   julia-mode
					   kivy-mode
					   ninja-mode
					   paradox
					   rust-mode
					   thrift
                       ac-js2
                       ace-jump-mode
                       ack-and-a-half
                       ag
                       anzu
                       apache-mode
                       back-button
                       browse-kill-ring
					   php-boris
                       buffer-move
                       clojure-mode
                       cmake-mode
                       coffee-mode
                       company
                       csharp-mode
                       csv-nav
                       cycbuf
                       d-mode
                       dart-mode
                       debian-changelog-mode
                       diff-hl
                       dired+
                       dirtree
                       dos
                       ecb
                       editorconfig
                       el-get
                       elpy
                       erlang
                       expand-region
                       feature-mode
                       fic-ext-mode
                       flx-ido
                       flycheck
                       free-keys
                       fuzzy
                       gitconfig-mode
                       go-mode
					   god-mode
                       google-this
					   google-translate
                       grizzl
                       groovy-mode
                       guide-key-tip
                       haml-mode
                       haskell-mode
                       helm
                       helm-ack
                       helm-ag
                       helm-chrome
                       helm-company
                       helm-descbinds
                       helm-flycheck
                       helm-git-files
                       helm-git-grep
                       helm-google
                       helm-gtags
                       helm-projectile
                       helm-pydoc
                       helm-swoop
                       helm-themes
                       highlight-symbol
                       howdoi
                       htmlize
                       httprepl
                       ido-vertical-mode
                       iedit
                       iflipb
                       jedi
                       jinja2-mode
                       jquery-doc
                       js2-mode
                       json-mode
                       less-css-mode
					   let-alist
					   lua-mode
                       magit
					   magit-filenotify
                       manage-minor-mode
                       mark-tools
                       markdown-mode
                       matlab-mode
                       minimap
                       move-text
                       multi-term
                       multiple-cursors
                       nlinum
                       nyan-mode
                       persp-projectile
                       perspective
					   php-eldoc
                       php-mode
                       pkgbuild-mode
                       popwin
                       projectile
                       rainbow-delimiters
                       rainbow-mode
                       recentf-ext
                       ruby-mode
                       scala-mode
                       scss-mode
                       sequential-command
                       skewer-mode
                       slim-mode
                       slime
                       smart-mode-line
                       smartparens
					   sqlup-mode
                       string-inflection
                       stylus-mode
                       sublime-themes
                       sublimity
                       sunrise-commander
                       syslog-mode
                       textile-mode
                       tidy
                       tuareg
                       twig-mode
                       undo-tree
                       vimrc-mode
                       volatile-highlights
                       web-mode
                       wgrep
                       wgrep-helm
                       whitespace-cleanup-mode
                       windresize
                       yaml-mode
                       yascroll
                       yasnippet))
  
  (require 'package)

  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("SC"   . "http://joseito.republika.pl/sunrise-commander/") t)

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))
  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )


;; CUSTOMIZE :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-max-one-liner-length 100)
 '(c-report-syntactic-errors t)
 '(comint-buffer-maximum-size 10240)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-prompt-read-only t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 2)
 '(company-tooltip-limit 30)
 '(company-tooltip-minimum-width 1000)
 '(company-tooltip-offset-display (quote lines))
 '(compilation-ask-about-save nil)
 '(compilation-mode-hook nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote ("")))
 '(compilation-window-height 30)
 '(confirm-kill-emacs nil)
 '(custom-safe-themes
   (quote
	("52849d123c41baae998e6ede45b8af06fae6fc6a2171c7dd80876862a87b936f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(debug-on-error nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(develock-auto-enable nil)
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(dired-auto-revert-buffer t)
 '(ecb-add-path-for-not-matching-files (quote (t)))
 '(ecb-auto-expand-tag-tree (quote all))
 '(ecb-layout-name "left3")
 '(ecb-layout-window-sizes
   (quote
	(("left3"
	  (ecb-directories-buffer-name 0.11406844106463879 . 0.2903225806451613)
	  (ecb-sources-buffer-name 0.11406844106463879 . 0.3387096774193548)
	  (ecb-methods-buffer-name 0.11406844106463879 . 0.3548387096774194))
	 ("left15"
	  (ecb-directories-buffer-name 0.1444866920152091 . 0.5967741935483871)
	  (ecb-methods-buffer-name 0.1444866920152091 . 0.3870967741935484))
	 ("left13"
	  (ecb-directories-buffer-name 0.12213740458015267 . 0.9833333333333333)))))
 '(ecb-methods-menu-sorter nil)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-file-regexps
   (quote
	((".*"
	  ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\)$\\)\\)")
	  ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path
   (quote
	(("/home/k/proyectos" "proyectos")
	 (#("/home/k/sandbox" 0 15
		(help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))
	  "sandbox")
	 ("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-toggle-layout-sequence
   (quote
	("left1" "left2" "left3" "left4" "left5" "left6" "left7" "left8" "left9" "left10" "left11" "left12" "left13" "left14" "left15" "right1" "right2" "leftright1" "leftright2" "leftright3" "leftright-analyse" "left-analyse" "left-symboldef" "left-dir-plus-speedbar")))
 '(ecb-windows-width 0.25)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(electric-indent-mode t)
 '(elpy-default-minor-modes (quote (eldoc-mode)))
 '(elpy-rpc-backend "rope")
 '(elpy-rpc-project-specific t)
 '(enable-remote-dir-locals t)
 '(eval-expression-debug-on-error nil)
 '(flx-ido-mode t)
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(flycheck-completion-system nil)
 '(flymake-run-in-place nil)
 '(font-lock-maximum-decoration 5)
 '(git-commit-fill-column 9999)
 '(git-commit-mode-hook (quote (flyspell-mode)))
 '(git-commit-summary-max-length 9999)
 '(global-anzu-mode t)
 '(global-diff-hl-mode t)
 '(global-fixmee-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(global-hl-line-mode t)
 '(global-prettify-symbols-mode t)
 '(global-semantic-idle-breadcrumbs-mode t nil (semantic/idle))
 '(global-superword-mode t)
 '(global-undo-tree-mode t)
 '(global-yascroll-bar-mode t)
 '(google-this-mode t)
 '(grep-files-aliases nil)
 '(gud-tooltip-echo-area t)
 '(gud-tooltip-mode t)
 '(guide-key-mode t)
 '(guide-key-tip/enabled t)
 '(helm-buffer-details-flag nil)
 '(helm-buffer-max-length nil)
 '(helm-buffers-fuzzy-matching t)
 '(helm-c-ack-version 2.04)
 '(helm-descbinds-mode t)
 '(highlight-indentation-offset 2)
 '(history-length 100)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-eliding-string "..")
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(ido-vertical-mode t)
 '(iflipb-wrap-around t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "english")
 '(js-flat-functions t)
 '(js2-mode-show-parse-errors nil)
 '(kill-ring-max 200)
 '(magit-use-overlays nil)
 '(make-backup-files t)
 '(mk-proj-use-ido-selection t)
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (4 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(next-error-recenter (quote (4)))
 '(ns-command-modifier (quote meta))
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-default-notes-file "~/.notes.org")
 '(ourcomments-ido-ctrl-tab t)
 '(paradox-github-token t)
 '(powerline-text-scale-factor 0.8)
 '(proced-auto-update-interval 1)
 '(projectile-completion-system (quote completing-read))
 '(projectile-global-mode t)
 '(pydb-many-windows nil)
 '(pydb-pydbtrack-do-tracking-p t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(remember-data-file "~/.agenda.org")
 '(rm-blacklist
   " hl-p\\| FIC\\| yas\\| VHl\\| Helm\\| AC\\| SP\\| hl-s\\| ||\\| Google\\| WSC\\| ws\\| UT\\| company\\| back\\| Anzu\\| Guide\\| hs\\| Projectile\\[?.*\\]?")
 '(rm-excluded-modes
   " hl-p\\| FIC\\| yas\\| VHl\\| Helm\\| AC\\| SP\\| hl-s\\| ||\\| Google\\| WSC\\| ws\\| UT\\| company\\| back\\| Anzu\\| Guide\\| hs\\| Projectile\\[?.*\\]?")
 '(ropemacs-completing-read-function (quote completing-read))
 '(ropemacs-enable-shortcuts nil)
 '(ropemacs-local-prefix nil)
 '(save-interprogram-paste-before-kill nil)
 '(save-place t nil (saveplace))
 '(savehist-additional-variables
   (quote
	(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history compile-command)))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(semantic-idle-scheduler-idle-time 1)
 '(send-mail-function (quote mailclient-send-it))
 '(show-smartparens-global-mode t)
 '(show-trailing-whitespace nil)
 '(smart-tab-using-hippie-expand t)
 '(smartparens-global-mode t)
 '(smex-history-length 30)
 '(sml/hidden-modes
   " hl-p\\| FIC\\| yas\\| VHl\\| Helm\\| AC\\| SP\\| hl-s\\| ||\\| Google\\| WSC\\| ws\\| UT\\| company\\| back\\| Anzu\\| Guide\\| hs\\| Projectile\\[?.*\\]?")
 '(sml/theme (quote dark))
 '(sml/vc-mode-show-backend t)
 '(sp-autoescape-string-quote nil)
 '(speedbar-show-unknown-files t)
 '(speedbar-track-mouse-flag t)
 '(speedbar-update-flag nil)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(sublimity-mode t)
 '(sublimity-scroll-drift-length 1)
 '(sublimity-scroll-weight 7)
 '(svn-log-edit-show-diff-for-commit t)
 '(svn-status-default-blame-arguments (quote ("-x" "--ignore-eol-style" "-g")))
 '(svn-status-default-log-arguments (quote ("-v -g")))
 '(svn-status-verbose t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40)))
 '(tab-width 4)
 '(tabkey2-completion-lighter-on t)
 '(tabkey2-first-key "  ")
 '(tabkey2-mode t)
 '(tabkey2-show-message-on-enter 2.0)
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout nil)
 '(tramp-remote-path (quote (tramp-default-remote-path tramp-own-remote-path)))
 '(tramp-verbose 5)
 '(truncate-lines t)
 '(undo-tree-mode-lighter " UT")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box nil)
 '(visible-bell t)
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-default-display-inline-images t)
 '(w3m-use-cookies t)
 '(web-mode-disable-auto-pairing t)
 '(wget-download-directory "~/Downloads")
 '(winner-mode t nil (winner)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(comint-highlight-input ((t nil)))
 '(ecb-default-general-face ((t (:height 0.9)))))



;; GENERAL:
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


(add-to-list 'load-path "~/.emacs.d/my-elisp/")
(require 'cl-lib)
(require 'guide-key-tip)
(require 'smartparens-config)
(require 'smartparens-html)
(require 'volatile-highlights)
(require 'move-text)
(require 'sublimity)
(require 'sublimity-scroll)
(require 'company)
;; (require 'sublimity-map)
(defalias 'yes-or-no-p 'y-or-n-p)
(load-library "iso-transl")
(toggle-diredp-find-file-reuse-dir 1)
(setq yas-verbosity 1)
(setq desktop-dirname "~/.emacs.d")
(setq bookmark-default-file "~/.emacs.d/bookmarks.emacs")
(setq tramp-debug-buffer t)
(setq-default ediff-auto-refine "on")
(setq-default cursor-type 'bar) 
(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq web-mode-engines-alist '(( "django" . "\\.html$")))
;; (setq jedi:complete-on-dot t)                 ; optional

(setq sml/vc-mode-show-backend t)
;; (add-to-list 'company-backends 'company-anaconda)
(put 'erase-buffer 'disabled nil)

(setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-x 4" "C-x 5" "C-c" "C-c p" "C-c p 4""C-c /" "C-c ." "C-c . l" "C-c . g" "C-c . m" "C-c &" "C-x c" "C-c !" "C-c C-t" "C-c C-e" "C-c C-d" "C-c C-b" "C-x x" "C-c @" "C-c C-t" "C-x p" "C-x p n" "C-x p 4"))
(volatile-highlights-mode t)

;; Disable version control in tramp:
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))


(setq-default frame-title-format
              (list '((buffer-file-name "%f - emacs" (dired-directory
                                                      dired-directory
                                                      (revert-buffer-function " %b"
                                                                              ("%b – Dir:  " default-directory)))))))





(defadvice yank (before slick-copy activate)
  "Position point when yanking lines."
  (let ((kill (current-kill 0 t)))
 (when (eq ?\n (elt kill (1- (length kill))))
             (beginning-of-line))))




(require 'dopemacs-elisp)


;; HOOKS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun dopemacs-text-mode-hook ()
  (nlinum-mode)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")
  (local-set-key "\C-j"  'join-line)
  (local-set-key (kbd "RET")  'newline-and-indent)
  )

(defun dopemacs-prog-mode-hook ()
  (company-mode)
  (yas-minor-mode)
  ;; (dopemacs-font-lock-operators)
  ;; (rainbow-delimiters-mode)
  (highlight-symbol-mode)
  (which-func-mode)
  (volatile-highlights-mode)
  (nlinum-mode)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")
  (local-set-key "\C-j"  'join-line)
  (local-set-key (kbd "RET")  'newline-and-indent)
  (ignore-errors (hs-minor-mode))
  ;;  (whitespace-mode)
  (semantic-mode)
  )

(add-hook 'prog-mode-hook 'dopemacs-prog-mode-hook)
(add-hook 'text-mode-hook 'dopemacs-text-mode-hook)
(add-hook 'text-mode-hook 'dopemacs-fixed-width-font)
;; (add-hook 'tabulated-list-mode-hook 'dopemacs-fixed-width-font)
(add-hook 'comint-mode-hook 'dopemacs-fixed-width-font)
;; (add-hook 'special-mode-hook 'dopemacs-fixed-width-font)
(add-hook 'dired-mode-hook 'dopemacs-fixed-width-font)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'ediff-mode-hook 'dopemacs-fixed-width-font)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc)

;; Workaround for bug of ecb and winner-mode:
;; http://stackoverflow.com/questions/9389679/how-to-unload-a-mode-e-g-unload-ecb-to-restore-winner-el-functionality
(add-hook 'ecb-deactivate-hook '(lambda () (ecb-disable-advices 'ecb-winman-not-supported-function-advices t)))






;; ASSOCIATIONS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.ui$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . nxml-mode))
(add-to-list 'auto-mode-alist '("configure\\(\\.in\\)?\\'" . autoconf-mode))
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\.twig$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))





;; KEYBINDINGS:
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


 (global-set-key "\M-x" 'helm-M-x)
(global-set-key (kbd "<menu>") 'er/expand-region)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-select-action)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))


(windmove-default-keybindings 'meta)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "<menu>") 'save-buffer)
(global-set-key (kbd "<C-menu>") 'save-buffer)
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

(global-set-key (kbd "<C-prior>") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-next>") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'iflipb-previous-buffer)

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "M-w") 'dopemacs-xah-copy-line-or-region)
(global-set-key (kbd "C-w") 'dopemacs-xah-cut-line-or-region)
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "M-K") 'delete-frame)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is your old M-x.
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "M-_") 'undo-tree-redo)
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-º") 'company-complete)
(global-set-key (kbd "C-x t") 'anchored-transpose)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "C-ñ") 'helm-mini)
(global-set-key (kbd "C-Ñ") 'hs-toggle-hiding)
(global-set-key (kbd "<C-dead-acute>") (lambda () (interactive) (repeat-complex-command 0)))
(global-set-key (kbd "<C-dead-grave>") 'helm-projectile)
(global-set-key (kbd "<C-menu>") 'helm-projectile)

(global-set-key "\C-ca" 'projectile-ag)
(global-set-key "\C-cb" 'magit-blame-mode)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-ce" 'er/expand-region)
(global-set-key "\C-cf" 'helm-recentf)
(global-set-key "\C-cg" 'rgrep)
(global-set-key "\C-ch" 'hs-toggle-hiding)
(global-set-key "\C-ci" 'string-inflection-cycle)
(global-set-key "\C-cj" 'dired-at-point)
(global-set-key "\C-ck" 'dopemacs-select-current-line)
(global-set-key "\C-cl" 'google-this-lucky-search)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cn" 'manage-minor-mode)
(global-set-key "\C-co" 'magit-log)
;; (global-set-key "\C-cp") ;; reserved for Projectile
(global-set-key "\C-cq" 'google-translate-at-point)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cs" 'multi-term)
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-cu" 'dopemacs-swap-windows)
(global-set-key "\C-cv" 'eval-buffer)
(global-set-key "\C-cw" 'dopemacs-toggle-window-split)
(global-set-key "\C-cx" 'dopemacs-split-window)


(global-set-key (kbd "<f5>") 'menu-bar-mode)
(global-set-key (kbd "<f6>") 'windresize)
(global-set-key (kbd "<f7>") 'ecb-toggle-layout)
(global-set-key (kbd "<f8>") 'ecb-minor-mode)
(global-set-key (kbd "<f9>") 'sr-speedbar-toggle)
(global-set-key (kbd "<f11>") 'dopemacs-toggle-fullscreen)
(global-set-key (kbd "<f12>") 'sunrise)



;; SMARTPARENS BINDINGS:
;;;;;;;;;
;; global

(require 'smartparens-config)
;; (require 'smartparens-html)




(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

;; (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
;; 
(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;; (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

;; (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object) 
;; (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
;; (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
;; (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
;; (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
;; (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
;; (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
;; (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
;; (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))


;; INITIALIZATIONS
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

(load "server")
(unless (server-running-p) (server-start))


(cond ((eq window-system 'x)
       (dopemacs-toggle-fullscreen)))

(helm-mode 1)

(sml/setup)
;; (elpy-enable)
;; (delq 'flymake-mode elpy-default-minor-modes)
(load-theme 'my-zenburn)





(provide 'init)
;;; init.el ends here




(put 'downcase-region 'disabled nil)
