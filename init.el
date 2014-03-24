;;; init.el --- Initialization
;;; -*- coding: utf-8 -*-

;; PACKAGES :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------


(when (>= emacs-major-version 24)
  (setq package-list '(
                       ack-and-a-half
                       apache-mode
                       idle-highlight
                       anzu
                       volatile-highlights
                       haml-mode
                       auto-complete
                       flx-ido
                       dired+
                       haskell-mode
                       better-defaults
                       debian-changelog-mode
                       browse-kill-ring
                       move-text
                       expand-region
                       mark-tools
                       whitespace-cleanup-mode
                       manage-minor-mode
                       cmake-mode
                       rainbow-delimiters
                       clojure-mode
                       matlab-mode
                       ;; smartrep
                       undo-tree
                       jedi
                       powerline
                       diff-hl
                       htmlize
                       dos
                       nyan-mode
                       multiple-cursors
                       minimap
                       guide-key
                       scala-mode
                       smartparens
                       recentf-ext
                       popwin
                       jquery-doc
                       go-mode
                       ruby-mode
                       coffee-mode
                       zenburn-theme
                       iflipb
                       google-this
                       csharp-mode
                       csv-mode
                       cursor-chg
                       ecb
                       ido-vertical-mode
                       cycbuf
                       editorconfig
                       vimrc-mode
                       feature-mode
                       fuzzy
                       fic-ext-mode
                       find-file-in-project
                       flycheck
                       free-keys
                       guess-offset
                       helm
                       ido-ubiquitous
                       js2-mode
                       json-mode
                       less-css-mode
                       magit
                       markdown-mode
                       nav
                       php-mode
                       projectile
                       smart-mode-line
                       rainbow-mode
                       scss-mode
                       smex
                       sr-speedbar
                       stylus-mode
                       syslog-mode
                       web-mode
                       wgrep
                       multi-term
                       yaml-mode
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
 '(ack-and-a-half-arguments (quote ("--ignore-dir=static/vendor --ignore-dir=static/bower_components")))
 '(ack-and-a-half-executable "ack-grep")
 '(ack-executable (executable-find "ack-grep"))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "linux") (c++-mode . "linux") (java-mode . "java") (awk-mode . "awk") (other . "linux"))))
 '(c-max-one-liner-length 100)
 '(c-report-syntactic-errors t)
 '(canlock-password "339da088f721539f5d6ab06b3b2dcf98e112f3ad")
 '(comint-buffer-maximum-size 10240)
 '(comint-move-point-for-output t)
 '(compilation-ask-about-save nil)
 '(compilation-mode-hook nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote ("")))
 '(compilation-window-height 30)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("16248150e4336572ff4aa21321015d37c3744a9eb243fbd1e934b594ff9cf394" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(debug-on-error nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(develock-auto-enable nil)
 '(dired-auto-revert-buffer t)
 '(ecb-add-path-for-not-matching-files (quote (t)))
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes (quote (("left15" (ecb-directories-buffer-name 0.13215859030837004 . 0.6333333333333333) (ecb-methods-buffer-name 0.13215859030837004 . 0.35)) ("left7" (ecb-directories-buffer-name 0.14096916299559473 . 0.5833333333333334) (ecb-history-buffer-name 0.14096916299559473 . 0.15) (ecb-methods-buffer-name 0.14096916299559473 . 0.25)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path (quote (("/home/k/sandbox" "sandbox") ("/home/k" "home") ("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(electric-indent-mode t)
 '(elpy-rpc-backend "jedi")
 '(enable-remote-dir-locals t)
 '(erc-nick "paseante")
 '(eval-expression-debug-on-error nil)
 '(flx-ido-mode t)
 '(flymake-run-in-place nil)
 '(global-font-lock-mode t)
 '(global-hl-line-mode nil)
 '(grep-files-aliases nil)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ".libs")))
 '(grep-find-ignored-files (quote (".#*" "*.rbc" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "TAGS")))
 '(gud-tooltip-echo-area t)
 '(gud-tooltip-mode t)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(highlight-indentation-offset 2)
 '(history-length 100)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-eliding-string "..")
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(ido-use-virtual-buffers t)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(ido-vertical-mode t)
 '(iflipb-wrap-around t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(linum-format "%d ")
 '(magit-use-overlays nil)
 '(make-backup-files t)
 '(mk-proj-use-ido-selection t)
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(next-error-recenter (quote (4)))
 '(ns-command-modifier (quote meta))
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-todo-list-sublevels nil)
 '(org-default-notes-file "~/.notes.org")
 '(org-todo-keywords (quote ((sequence "TODO" "|" "DISCARDED" "DELEGATED" "DONE"))))
 '(ourcomments-ido-ctrl-tab t)
 '(proced-auto-update-interval 1)
 '(projectile-enable-caching t)
 '(pydb-many-windows nil)
 '(pydb-pydbtrack-do-tracking-p t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 1000)
 '(recentf-mode t)
 '(remember-data-file "~/.agenda.org")
 '(save-place t nil (saveplace))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10)
 '(semantic-idle-scheduler-idle-time 1)
 '(show-paren-mode t)
 '(show-smartparens-global-mode t)
 '(show-trailing-whitespace t)
 '(smart-tab-using-hippie-expand t)
 '(smartparens-global-mode t)
 '(smex-history-length 30)
 '(sml/mode-width 40)
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
 '(visible-bell t)
 '(w3m-default-display-inline-images t)
 '(w3m-use-cookies t)
 '(web-mode-code-indent-offset 4)
 '(web-mode-disable-auto-pairing t)
 '(wget-download-directory "~/Downloads")
 '(whitespace-style (quote (space-before-tab indentation space-after-tab)))
 '(winner-mode t nil (winner))
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(ecb-default-general-face ((t (:height 0.9))) t)
 '(idle-highlight ((t (:underline t)))))



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
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq web-mode-engines-alist '(( "django" . "\\.html$")))
(setq jedi:complete-on-dot t)                 ; optional
(require 'smartparens-config)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(require 'move-text)

;; (require 'smartparens-html)


(require 'python)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(setq-default frame-title-format
              (list '((buffer-file-name "emacs - %f" (dired-directory
                                                      dired-directory
                                                      (revert-buffer-function " %b"
                                                                              ("%b – Dir:  " default-directory)))))))


(require 'dopemacs-elisp)

;; (delete-file tramp-persistency-file-name) ;; para que no cachee mierdas


;; (setq vc-ignore-dir-regexp
;;                 (format "\\(%s\\)\\|\\(%s\\)"
;;                         vc-ignore-dir-regexp
;;                         tramp-file-name-regexp))



(defadvice yank (before slick-copy activate)
  "Position point when yanking lines."
  (let ((kill (current-kill 0 t)))
 (when (eq ?\n (elt kill (1- (length kill))))
             (beginning-of-line))))

;; HOOKS :
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------




(defun dopemacs-prog-mode-hook ()
  (yas-minor-mode)
  (fic-ext-mode)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")
  ;; (modify-syntax-entry ?< "(>")
  ;; (modify-syntax-entry ?> ")<")
  (font-lock-mode)
  ;; (guess-style-guess-all)
  (linum-mode)
  (local-set-key "\C-j"  'dopemacs-unir-lineas)
  (local-set-key "\C-t"  'dopemacs-copiar-linea)
  (local-set-key (kbd "RET")  'newline-and-indent)
  (idle-highlight)
  (whitespace-mode)
  (volatile-highlights-mode)
  ;; (smart-tab-mode)
  )


;; el cogollo del meollo:
(add-hook 'prog-mode-hook 'dopemacs-prog-mode-hook)

(require 'jquery-doc)
;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'js2-mode-hook 'jquery-doc-setup)
(add-hook 'web-mode-hook  'jquery-doc-setup)
(add-hook 'json-mode 'flymake-json-load)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))

;; WORKAROUND para bug de ecb y winner-mode: http://stackoverflow.com/questions/9389679/how-to-unload-a-mode-e-g-unload-ecb-to-restore-winner-el-functionality
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







;; KEYBINDINGS:
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------

;; (ffap-bindings)
;; (global-set-key "\M-x" 'helm-M-x)

(windmove-default-keybindings 'meta)


(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

(global-set-key (kbd "<C-prior>") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-next>") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'iflipb-previous-buffer)

(global-set-key (kbd "M-w") 'dopemacs-xah-copy-line-or-region)
(global-set-key (kbd "C-w") 'dopemacs-xah-cut-line-or-region)
(global-set-key (kbd "M-k") 'dopemacs-kill-this-buffer)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is your old M-x.
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "M-_") 'undo-tree-redo)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key " " (quote hippie-expand)) ;; M-TAB
(global-set-key "\C-cb" 'browse-url)
(global-set-key "\C-cp" 'dopemacs-find-file-in-repo)
(global-set-key "\C-cg" 'rgrep)
(global-set-key "\C-ch" 'helm-mini)
(global-set-key "\C-cj" 'dired-at-point)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cs" 'svn-status)
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-cv" 'eval-buffer)
(global-set-key "\C-cw" 'dopemacs-toggle-window-split)
(global-set-key "\C-cw" 'dopemacs-toggle-window-split)
(global-set-key "\C-cf" 'dopemacs-recentf-ido-find-file)
(global-set-key "\M-o" 'other-window)
;; (global-set-key (kbd "C-!") 'dopemacs-babcore-shell-execute)
(global-set-key (kbd "<C-M-backspace>") 'dopemacs-backward-kill-line)

;; (global-set-key (kbd "M-ç") "}")
;; (global-set-key "\C-ch" 'idle-highlight)
(global-set-key "\C-cl" 'package-list-packages-no-fetch)

;;(global-set-key (kbd "<C-tab>") 'dopemacs-switch-to-buffer-quick)
(global-set-key (kbd "<f6>") '(lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "<f7>") 'ecb-minor-mode)
(global-set-key (kbd "<f8>") 'sunrise)

(global-set-key (kbd "<C-kp-2>") 'shrink-window)
(global-set-key (kbd "<C-kp-8>") 'enlarge-window)
(global-set-key (kbd "<C-kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-6>") 'enlarge-window-horizontally)

;; (global-set-key (kbd "<f9>") 'gud-cont)
;; (global-set-key (kbd "<f11>") 'gud-step)
;; (global-set-key (kbd "<f12>") 'gud-next)
;; (global-set-key "\C-y" '(lambda () (interactive) (yank nil)
;; (global-set-key "\C-c\C-h" '(lambda () (interactive) (highlight-regexp (thing-at-point 'word))))
;; (global-set-key (kbd "S-<next>") 'scroll-up)
;; (global-set-key (kbd "S-<prior>") 'scroll-down)
;; (global-set-key "\C-ch" 'highlight-symbol-at-point)
;; (global-set-key "\C-cn" 'highlight-symbol-next)
;; (global-set-key "\C-cp" 'highlight-symbol-prev)
;; (global-set-key "\C-cq" 'highlight-symbol-query-replace)


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

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

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


;; (helm-mode)
;; (sml/setup)
(powerline-default-theme)
;; (ecb-activate)
(global-auto-complete-mode)
(global-rainbow-delimiters-mode)
(global-undo-tree-mode)
(global-diff-hl-mode)
(global-flycheck-mode)
;; (elpy-enable)
(global-whitespace-cleanup-mode)
(projectile-global-mode)
(global-anzu-mode +1)









(provide 'init)
;; init.el ends here




