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
					   helm-ag
					   helm-google
					   jinja2-mode
					   smart-mode-line
					   sunrise-commander
					   tidy
					   yasnippet
					   ;; fixmee
					   ;; virtualenvwrapper
					   ag
					   back-button
					   buffer-move
					   company
					   dart-mode
					   el-get
					   elpy
					   gitconfig-mode
					   grizzl
					   groovy-mode
					   guide-key-tip
					   helm-ack
					   helm-chrome
					   helm-flycheck
					   helm-git-files
					   helm-git-grep
					   helm-google
					   helm-gtags
					   helm-pydoc
					   helm-swoop
					   helm-themes
					   highlight-symbol
					   howdoi
					   iedit
					   nlinum
					   pkgbuild-mode
					   rainbow-mode
					   sequential-command
					   slim-mode
					   slime
					   textile-mode
					   tuareg
					   w3m
                       ;; ac-js2
                       ;; auto-complete
                       ;; rainbow-mode
                       ;; smartrep
                       ack-and-a-half
                       anzu
                       apache-mode
                       browse-kill-ring
                       clojure-mode
                       cmake-mode
                       coffee-mode
                       csharp-mode
                       csv-nav
                       cycbuf
                       d-mode
                       debian-changelog-mode
                       diff-hl
                       dired+
                       dos
                       ecb
                       editorconfig
                       erlang
                       expand-region
                       feature-mode
                       fic-ext-mode
                       find-file-in-project
                       flx-ido
                       flycheck
                       free-keys
                       fuzzy
                       go-mode
                       google-this
                       guess-offset
                       haml-mode
                       haskell-mode
                       helm
                       helm-descbinds
                       helm-projectile
                       htmlize
                       ido-ubiquitous
                       ido-vertical-mode
                       iflipb
                       jedi
                       jquery-doc
                       js2-mode
                       json-mode
                       less-css-mode
                       magit
                       manage-minor-mode
                       mark-tools
                       markdown-mode
                       matlab-mode
                       minimap
                       move-text
                       multi-term
                       multiple-cursors
                       nav
                       nyan-mode
                       php-mode
                       popwin
                       powerline
                       projectile
                       rainbow-delimiters
                       recentf-ext
                       ruby-mode
                       scala-mode
                       scss-mode
                       skewer-mode
                       smartparens
                       smex
                       sr-speedbar
                       stylus-mode
                       syslog-mode
                       undo-tree
                       vimrc-mode
                       volatile-highlights
                       web-mode
                       wgrep
                       whitespace-cleanup-mode
                       windresize
                       yaml-mode
                       zenburn-theme))
  
  (require 'package)

  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
 '(ack-and-a-half-arguments (quote ("--ignore-dir=static/vendor --ignore-dir=static/bower_components")))
 '(ack-and-a-half-executable "ack-grep")
 '(ack-executable (executable-find "ack-grep"))
 '(back-button-mode t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "linux") (c++-mode . "linux") (java-mode . "java") (awk-mode . "awk") (other . "linux"))))
 '(c-max-one-liner-length 100)
 '(c-report-syntactic-errors t)
 '(canlock-password "339da088f721539f5d6ab06b3b2dcf98e112f3ad")
 '(comint-buffer-maximum-size 10240)
 '(comint-move-point-for-output t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 2)
 '(company-tooltip-limit 30)
 '(compilation-ask-about-save nil)
 '(compilation-mode-hook nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote ("")))
 '(compilation-window-height 30)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" default)))
 '(debug-on-error nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(develock-auto-enable nil)
 '(dired-auto-revert-buffer t)
 '(ecb-add-path-for-not-matching-files (quote (t)))
 '(ecb-layout-name "left13")
 '(ecb-layout-window-sizes (quote (("left13" (ecb-directories-buffer-name 0.12213740458015267 . 0.9833333333333333)) ("left15" (ecb-directories-buffer-name 0.13215859030837004 . 0.6333333333333333) (ecb-methods-buffer-name 0.13215859030837004 . 0.35)) ("left7" (ecb-directories-buffer-name 0.14096916299559473 . 0.5833333333333334) (ecb-history-buffer-name 0.14096916299559473 . 0.15) (ecb-methods-buffer-name 0.14096916299559473 . 0.25)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path (quote (("/home/k/sandbox" "sandbox") ("/home/k" "home") ("/" "/") ("/home/k/proyectos" "proyectos"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(electric-indent-mode nil)
 '(elpy-rpc-backend "rope")
 '(enable-remote-dir-locals t)
 '(erc-nick "paseante")
 '(eval-expression-debug-on-error nil)
 '(flx-ido-mode t)
 '(flycheck-completion-system nil)
 '(flymake-run-in-place nil)
 '(global-anzu-mode t)
 '(global-diff-hl-mode t)
 '(global-fixmee-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(global-rainbow-delimiters-mode t)
 '(global-undo-tree-mode t)
 '(google-this-mode t)
 '(grep-files-aliases nil)
 '(gud-tooltip-echo-area t)
 '(gud-tooltip-mode t)
 '(guide-key-mode t)
 '(guide-key-tip/enabled t)
 '(helm-c-ack-version 2.04)
 '(helm-descbinds-mode t)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(highlight-indentation-offset 2)
 '(history-length 100)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-eliding-string "..")
 '(ido-enable-flex-matching t)
 '(ido-mode (quote file) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(ido-vertical-mode t)
 '(iflipb-wrap-around t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "english")
 '(magit-use-overlays nil)
 '(make-backup-files t)
 '(mk-proj-use-ido-selection t)
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(mouse-yank-at-point t)
 '(next-error-recenter (quote (4)))
 '(ns-command-modifier (quote meta))
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-default-notes-file "~/.notes.org")
 '(ourcomments-ido-ctrl-tab t)
 '(powerline-text-scale-factor 0.8)
 '(proced-auto-update-interval 1)
 '(projectile-completion-system (quote completing-read))
 '(projectile-global-mode t)
 '(pydb-many-windows nil)
 '(pydb-pydbtrack-do-tracking-p t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 1000)
 '(recentf-mode t)
 '(remember-data-file "~/.agenda.org")
 '(safe-local-variable-values (quote ((ack-and-a-half-arguments "--ignore-dir=static/vendor --ignore-dir=static/bower_components --ignore-dir=log") (web-mode-markup-indent-offset . 2) (web-mode-markup-indent-offset 2) (ack-and-a-half-arguments "--ignore-dir=static/vendor --ignore-dir=static/bower_components") (ack-and-a-half-arguments quote "--ignore-dir=static/vendor --ignore-dir=static/bower_components") (ack-and-a-half-arguments . "--ignore-dir=static/vendor --ignore-dir=static/bower_components") (ack-and-a-half-arguments quote ("--ignore-dir=static/vendor --ignore-dir=static/bower_components")))))
 '(save-interprogram-paste-before-kill nil)
 '(save-place t nil (saveplace))
 '(savehist-additional-variables (quote (kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history compile-command)))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(semantic-idle-scheduler-idle-time 1)
 '(show-smartparens-global-mode t)
 '(show-trailing-whitespace nil)
 '(smart-tab-using-hippie-expand t)
 '(smartparens-global-mode t)
 '(smex-history-length 30)
 '(sml/hidden-modes (quote (" hl-p" " FIC" " yas" " VHl" " Helm" " AC" " SP" " hl-s" " ||" " Google" " WSC" " ws" " UT" " company" " back" " Anzu" " Guide")))
 '(sml/use-projectile-p nil)
 '(sml/vc-mode-show-backend t)
 '(sp-autoescape-string-quote nil)
 '(speedbar-show-unknown-files t)
 '(speedbar-track-mouse-flag t)
 '(speedbar-update-flag nil)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(sr-speedbar-width-x 10)
 '(svn-log-edit-show-diff-for-commit t)
 '(svn-status-default-blame-arguments (quote ("-x" "--ignore-eol-style" "-g")))
 '(svn-status-default-log-arguments (quote ("-v -g")))
 '(svn-status-verbose t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40)))
 '(tab-width 4)
 '(tabkey2-completion-lighter-on t)
 '(tabkey2-first-key "	")
 '(tabkey2-mode t)
 '(tabkey2-show-message-on-enter 2.0)
 '(tool-bar-mode nil)
 '(tramp-default-method "rsync")
 '(tramp-remote-path (quote (tramp-default-remote-path tramp-own-remote-path)))
 '(tramp-verbose 5)
 '(truncate-lines t)
 '(undo-tree-mode-lighter " UT")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visible-bell t)
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-default-display-inline-images t)
 '(w3m-use-cookies t)
 '(web-mode-disable-auto-pairing t)
 '(wget-download-directory "~/Downloads")
 '(whitespace-style (quote (space-before-tab indentation space-after-tab)))
 '(winner-mode t nil (winner)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(ecb-default-general-face ((t (:height 0.9)))))



;; GENERAL:
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/my-elisp/")
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
(setq jedi:complete-on-dot t)                 ; optional
(setq helm-gtags-mode 1)
(setq sml/vc-mode-show-backend t)

(require 'guide-key-tip)
(setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-x 4" "C-x 5" "C-c" "C-c p" "C-c /" "C-c ." "C-c . l" "C-c . g" "C-c . m" "C-c &" "C-x c" "C-c !"))

(require 'smartparens-config)
(require 'smartparens-html)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(require 'move-text)


(setq-default frame-title-format
              (list '((buffer-file-name "emacs %f" (dired-directory
                                                      dired-directory
                                                      (revert-buffer-function " %b"
                                                                              ("%b – Dir:  " default-directory)))))))


(require 'dopemacs-elisp)



(defadvice yank (before slick-copy activate)
  "Position point when yanking lines."
  (let ((kill (current-kill 0 t)))
 (when (eq ?\n (elt kill (1- (length kill))))
             (beginning-of-line))))




;; HOOKS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun dopemacs-prog-mode-hook ()
	(yas-minor-mode)
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?- "w")
	;; (guess-style-guess-all)
	(nlinum-mode)
	(highlight-symbol-mode)
	(local-set-key "\C-j"  'join-line)
	(local-set-key (kbd "RET")  'newline-and-indent)
	(whitespace-mode)
	(volatile-highlights-mode)
	;; (which-function-mode)
    ;; (smart-tab-mode)
	)


(add-hook 'prog-mode-hook 'dopemacs-prog-mode-hook)

(defun dopemacs-text-mode-hook ()
	(set (make-local-variable 'face-remapping-alist)
                   '((default :family "DejaVu Sans Mono")))
	)

(add-hook 'text-mode-hook 'dopemacs-text-mode-hook)


(require 'jquery-doc)
;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'web-mode-hook 'jquery-doc-setup)
;; (add-hook 'web-mode-hook 'skewer-html-mode)
(add-hook 'js2-mode-hook 'jquery-doc-setup)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'json-mode 'flymake-json-load)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'python-mode-hook (lambda () (electric-indent-mode nil)))
(add-hook 'web-mode-hook (lambda ()(setq web-mode-markup-indent-offset 2)))

;; Workaround for bug of ecb and winner-mode:
;; http://stackoverflow.com/questions/9389679/how-to-unload-a-mode-e-g-unload-ecb-to-restore-winner-el-functionality
(add-hook 'ecb-deactivate-hook '(lambda () (ecb-disable-advices 'ecb-winman-not-supported-function-advices t)))







;; ASSOCIATIONS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
(add-to-list 'auto-mode-alist '("\\.ui$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl\\'$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'$" . nxml-mode))
(add-to-list 'auto-mode-alist '("configure\\(\\.in\\)?\\'" . autoconf-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))







;; KEYBININGS:
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


(helm-mode 1)
(global-set-key "\M-x" 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-mini)
;; (define-key global-map [remap find-file] 'helm-find-files)
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

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

(global-set-key (kbd "<C-prior>") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-next>") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'iflipb-previous-buffer)

(global-set-key (kbd "M-w") 'dopemacs-xah-copy-line-or-region)
(global-set-key (kbd "C-w") 'dopemacs-xah-cut-line-or-region)
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "M-K") 'delete-frame)


(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is your old M-x.
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "M-_") 'undo-tree-redo)
(global-set-key (kbd "C-ñ") 'helm-mini)
(global-set-key "\M-o" 'other-window)
(global-set-key (kbd "<C-M-backspace>") 'dopemacs-backward-kill-line)

(global-set-key " " (quote hippie-expand)) ;; M-TAB

(global-set-key "\C-cg" 'rgrep)
(global-set-key "\C-ca" 'projectile-ag)
(global-set-key "\C-cj" 'dired-at-point)
(global-set-key "\C-cb" 'magit-blame-mode)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cn" 'manage-minor-mode)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cs" 'multi-term)
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-cw" 'dopemacs-toggle-window-split)
(global-set-key "\C-cv" 'eval-buffer)
(global-set-key "\C-cf" 'helm-recentf)
(global-set-key "\C-cl" 'google-lucky-search)
(global-set-key "\C-ce" 'package-list-packages-no-fetch) ;; e of ELPA
(global-set-key "\C-c\C-n" 'highlight-symbol-next)
(global-set-key "\C-c\C-p" 'highlight-symbol-prev)
(global-set-key "\C-cñ" 'dopemacs-split-window)


(global-set-key (kbd "<f6>") 'windresize)
(global-set-key (kbd "<f7>") 'ecb-minor-mode)
(global-set-key (kbd "<f8>") 'sunrise)


;; (global-set-key (kbd "<f9>") 'gud-cont)
;; (global-set-key (kbd "<f11>") 'gud-step)
;; (global-set-key (kbd "<f12>") 'gud-next)
;; (global-set-key "\C-c\C-h" '(lambda () (interactive) (highlight-regexp (thing-at-point 'word))))
;; (global-set-key (kbd "S-<next>") 'scroll-up)
;; (global-set-key (kbd "S-<prior>") 'scroll-down)
;; (global-set-key "\C-ch" 'highlight-symbol-at-point)
;; (global-set-key "\C-cq" 'highlight-symbol-query-replace)



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

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))


;; INITIALIZATIONS
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

(load "server")
(unless (server-running-p) (server-start))


(cond ((eq window-system 'x)
       (dopemacs-toggle-fullscreen)))


(sml/setup)


(elpy-enable)
(delq 'flymake-mode elpy-default-minor-modes)





(provide 'init)
;;; init.el ends here


