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
                       ;; auto-complete
                       ;; fixmee
                       ;; rainbow-mode
                       ;; smartrep
                       ;; virtualenvwrapper
						  ;; anaconda-mode
						  ;; pymacs
                       ac-js2
                       ace-jump-mode
                       ack-and-a-half
                       ag
                       anzu
                       apache-mode
                       back-button
                       browse-kill-ring
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
                       google-this
                       grizzl
                       groovy-mode
                       guess-offset
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
                       magit
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
                       php-mode
                       pkgbuild-mode
                       perspective
                       persp-projectile
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
                       string-inflection
                       stylus-mode
                       sublimity
                       sublime-themes
                       sunrise-commander
                       syslog-mode
                       textile-mode
                       tidy
                       tuareg
                       undo-tree
                       vimrc-mode
                       volatile-highlights
                       w3m
                       web-mode
                       wgrep
                       wgrep-helm
                       whitespace-cleanup-mode
                       windresize
                       yaml-mode
                       yascroll
                       yasnippet))
  
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
 '(ack-and-a-half-executable "ack-grep")
 '(ack-executable (executable-find "ack-grep"))
 '(auto-hscroll-mode t)
 '(auto-save-default nil)
 '(back-button-mode t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-basic-offset 4 t)
 '(c-default-style (quote ((c-mode . "linux") (c++-mode . "linux") (java-mode . "java") (awk-mode . "awk") (other . "linux"))))
 '(c-max-one-liner-length 100)
 '(c-report-syntactic-errors t)
 '(comint-buffer-maximum-size 10240)
 '(comint-move-point-for-output t)
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
 '(custom-enabled-themes (quote (zenburn)))
 '(debug-on-error nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(develock-auto-enable nil)
 '(dired-auto-revert-buffer t)
 '(ecb-add-path-for-not-matching-files (quote (t)))
 '(ecb-layout-name "left13")
 '(ecb-layout-window-sizes (quote (("left13" (ecb-directories-buffer-name 0.12213740458015267 . 0.9833333333333333)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(electric-indent-mode nil)
 '(elpy-default-minor-modes (quote (eldoc-mode)))
 '(elpy-rpc-backend "jedi")
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
 '(global-undo-tree-mode t)
 '(global-yascroll-bar-mode t)
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
 '(js2-mode-show-parse-errors nil)
 '(magit-use-overlays nil)
 '(make-backup-files t)
 '(mk-proj-use-ido-selection t)
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (8 ((shift) . 1) ((control)))))
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
 '(save-interprogram-paste-before-kill nil)
 '(save-place t nil (saveplace))
 '(savehist-additional-variables (quote (kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history compile-command)))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(semantic-idle-scheduler-idle-time 1)
 '(show-smartparens-global-mode t)
 '(show-trailing-whitespace nil)
 '(smart-tab-using-hippie-expand t)
 '(smartparens-global-mode t)
 '(smex-history-length 30)
 '(sml/hidden-modes " hl-p\\| FIC\\| yas\\| VHl\\| Helm\\| AC\\| SP\\| hl-s\\| ||\\| Google\\| WSC\\| ws\\| UT\\| company\\| back\\| Anzu\\| Guide\\| hs\\| Projectile\\[.*\\]")
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
 '(tabkey2-first-key "	")
 '(tabkey2-mode t)
 '(tabkey2-show-message-on-enter 2.0)
 '(tool-bar-mode nil)
 '(tramp-remote-path (quote (tramp-default-remote-path tramp-own-remote-path)))
 '(tramp-verbose 5)
 '(truncate-lines t)
 '(undo-tree-mode-lighter " UT")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
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
 '(default ((t (:family "DejaVu Sans" :foundry "unknown" :slant normal :weight normal :height 100 :width normal))))
 '(company-tooltip ((t (:background "#4F4F4F" :foreground "#F0DFAF" :family "DejaVu Sans Mono"))))
 '(ecb-default-general-face ((t (:height 0.9))) t)
 '(highlight-symbol-face ((t (:background "dim gray")))))



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
(setq helm-gtags-mode 1)
(setq sml/vc-mode-show-backend t)
(add-to-list 'company-backends 'company-anaconda)

(setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-x 4" "C-x 5" "C-c" "C-c p" "C-c p 4""C-c /" "C-c ." "C-c . l" "C-c . g" "C-c . m" "C-c &" "C-x c" "C-c !" "C-c C-t" "C-c C-e" "C-c C-d" "C-c C-b" "C-x x" "C-c @" "C-c C-t" "C-x p" "C-x p n" "C-x p 4"))
(volatile-highlights-mode t)


(setq-default frame-title-format
              (list '((buffer-file-name "emacs %f" (dired-directory
                                                      dired-directory
                                                      (revert-buffer-function " %b"
                                                                              ("%b – Dir:  " default-directory)))))))


(require 'flycheck)
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (jsx-mode js-mode js2-mode web-mode))


(defadvice yank (before slick-copy activate)
  "Position point when yanking lines."
  (let ((kill (current-kill 0 t)))
 (when (eq ?\n (elt kill (1- (length kill))))
             (beginning-of-line))))




(require 'dopemacs-elisp)


;; HOOKS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun dopemacs-prog-mode-hook ()
    (company-mode)
	(yas-minor-mode)
	(nlinum-mode)
	(which-func-mode)
	;; (dopemacs-font-lock-operators)
	;; (rainbow-delimiters-mode)
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?- "w")
	(highlight-symbol-mode)
	(local-set-key "\C-j"  'join-line)
	(local-set-key (kbd "RET")  'newline-and-indent)
	;; (whitespace-mode)
	(volatile-highlights-mode)
	(hs-minor-mode)
	)

(add-hook 'prog-mode-hook 'dopemacs-prog-mode-hook)
(add-hook 'text-mode-hook 'dopemacs-fixed-width-font)
;; (add-hook 'special-mode-hook 'dopemacs-fixed-width-font)
(add-hook 'dired-mode-hook 'dopemacs-fixed-width-font)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc)
(add-hook 'jsx-mode-hook (lambda () (flycheck-select-checker 'jsxhint-checker) (flycheck-mode)))

;; Workaround for bug of ecb and winner-mode:
;; http://stackoverflow.com/questions/9389679/how-to-unload-a-mode-e-g-unload-ecb-to-restore-winner-el-functionality
(add-hook 'ecb-deactivate-hook '(lambda () (ecb-disable-advices 'ecb-winman-not-supported-function-advices t)))






;; ASSOCIATIONS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.ui$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl\\'$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'$" . nxml-mode))
(add-to-list 'auto-mode-alist '("configure\\(\\.in\\)?\\'" . autoconf-mode))
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))







;; KEYBINDINGS:
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


(global-set-key "\M-x" 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-mini)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-select-action)
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
(global-set-key (kbd "C-º") 'hippie-expand)
(global-set-key (kbd "M-SPC") 'company-complete)
(global-set-key (kbd "C-ñ") 'helm-mini)
(global-set-key (kbd "C-Ñ") 'hs-toggle-hiding)
(global-set-key (kbd "<C-dead-acute>") 'hs-toggle-hiding)
(global-set-key (kbd "<C-dead-grave>") 'helm-projectile)

(global-set-key "\C-ca" 'projectile-ag)
(global-set-key "\C-cb" 'magit-blame-mode)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-ce" 'package-list-packages-no-fetch)
(global-set-key "\C-cf" 'helm-recentf)
(global-set-key "\C-cg" 'rgrep)
(global-set-key "\C-ch" 'hs-toggle-hiding)
(global-set-key "\C-ci" 'string-inflection-cycle)
(global-set-key "\C-cj" 'dired-at-point)
(global-set-key "\C-cl" 'google-lucky-search)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cn" 'manage-minor-mode)
(global-set-key "\C-co" 'magit-log)
;; (global-set-key "\C-cp") ;; reserved for Projectile
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cs" 'multi-term)
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-cu" 'dopemacs-swap-windows)
(global-set-key "\C-cv" 'eval-buffer)
(global-set-key "\C-cw" 'dopemacs-toggle-window-split)
(global-set-key "\C-cx" 'dopemacs-split-window)


(global-set-key (kbd "<f6>") 'magit-status)
(global-set-key (kbd "<f7>") 'windresize)
(global-set-key (kbd "<f8>") 'ecb-minor-mode)
(global-set-key (kbd "<f9>") 'sr-speedbar-toggle)
(global-set-key (kbd "<f10>") 'sunrise)
(global-set-key (kbd "<f11>") 'dopemacs-toggle-fullscreen)



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
;; (load-theme 'jujube)
(load-theme 'my-zenburn)
(sml/setup)
;; (elpy-enable)
;; (delq 'flymake-mode elpy-default-minor-modes)






(provide 'init)
;;; init.el ends here


