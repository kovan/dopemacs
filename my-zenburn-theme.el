;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 20140418.13
;; X-Original-Version: 2.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on such this port
;; is based.

;;; Code:

(deftheme my-zenburn "The My-zenburn color theme")

;;; Color Palette

(defvar my-zenburn-colors-alist
  '(("my-zenburn-fg+1"     . "#FFFFEF")
    ;; ("my-zenburn-fg"       . "#DCDCCC")
    ("my-zenburn-fg"       . "#FFFFFF")
    ("my-zenburn-fg-1"     . "#656555")
    ("my-zenburn-bg-2"     . "#000000")
    ("my-zenburn-bg-1"     . "#2B2B2B")
    ("my-zenburn-bg-05"    . "#383838")
    ("my-zenburn-bg"       . "#3F3F3F")
    ("my-zenburn-bg+1"     . "#4F4F4F")
    ("my-zenburn-bg+2"     . "#5F5F5F")
    ("my-zenburn-bg+3"     . "#6F6F6F")
    ("my-zenburn-red+1"    . "#DCA3A3")
    ("my-zenburn-red"      . "#CC9393")
    ("my-zenburn-red-1"    . "#BC8383")
    ("my-zenburn-red-2"    . "#AC7373")
    ("my-zenburn-red-3"    . "#9C6363")
    ("my-zenburn-red-4"    . "#8C5353")
    ("my-zenburn-orange"   . "#DFAF8F")
    ("my-zenburn-yellow"   . "#F0DFAF")
    ("my-zenburn-yellow-1" . "#E0CF9F")
    ("my-zenburn-yellow-2" . "#D0BF8F")
    ("my-zenburn-green-1"  . "#5F7F5F")
    ("my-zenburn-green"    . "#7F9F7F")
    ("my-zenburn-green+1"  . "#8FB28F")
    ("my-zenburn-green+2"  . "#9FC59F")
    ("my-zenburn-green+3"  . "#AFD8AF")
    ("my-zenburn-green+4"  . "#BFEBBF")
    ("my-zenburn-cyan"     . "#93E0E3")
    ("my-zenburn-blue+1"   . "#94BFF3")
    ("my-zenburn-blue"     . "#8CD0D3")
    ("my-zenburn-blue-1"   . "#7CB8BB")
    ("my-zenburn-blue-2"   . "#6CA0A3")
    ("my-zenburn-blue-3"   . "#5C888B")
    ("my-zenburn-blue-4"   . "#4C7073")
    ("my-zenburn-blue-5"   . "#366060")
    ("my-zenburn-magenta"  . "#DC8CC3"))
  "List of My-zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro my-zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `my-zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   my-zenburn-colors-alist))
     ,@body))

;;; Theme Faces
(my-zenburn-with-color-variables
  (custom-theme-set-faces
   'my-zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,my-zenburn-yellow :underline t :weight normal))))
   `(link-visited ((t (:foreground ,my-zenburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg))))
   `(cursor ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-fg+1))))
   `(escape-glyph ((t (:foreground ,my-zenburn-yellow :normal t))))
   `(fringe ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg+1))))
   `(header-line ((t (:foreground ,my-zenburn-yellow
                                  :background ,my-zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,my-zenburn-bg-05))))
   `(success ((t (:foreground ,my-zenburn-green :weight normal))))
   `(warning ((t (:foreground ,my-zenburn-orange :weight normal))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,my-zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,my-zenburn-green))))
   `(compilation-error-face ((t (:foreground ,my-zenburn-red-1 :weight normal :underline t))))
   `(compilation-face ((t (:foreground ,my-zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,my-zenburn-blue))))
   `(compilation-info ((t (:foreground ,my-zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,my-zenburn-green))))
   `(compilation-line-face ((t (:foreground ,my-zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,my-zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,my-zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,my-zenburn-orange :weight normal :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,my-zenburn-green+2 :weight normal))))
   `(compilation-mode-line-fail ((t (:foreground ,my-zenburn-red :weight normal))))
   `(compilation-mode-line-run ((t (:foreground ,my-zenburn-yellow :weight normal))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,my-zenburn-fg))))
   `(grep-error-face ((t (:foreground ,my-zenburn-red-1 :weight normal :underline t))))
   `(grep-hit-face ((t (:foreground ,my-zenburn-blue))))
   `(grep-match-face ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(match ((t (:background ,my-zenburn-bg-1 :foreground ,my-zenburn-orange :weight normal))))
;;;;; isearch
   `(isearch ((t (:foreground ,my-zenburn-yellow-2 :weight normal :background ,my-zenburn-bg+2))))
   `(isearch-fail ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,my-zenburn-yellow-2 :weight normal :background ,my-zenburn-bg-05))))

   `(menu ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,my-zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,my-zenburn-green+1
                           :background ,my-zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(mode-line-inactive
     ((t (:foreground ,my-zenburn-green-1
                      :background ,my-zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,my-zenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,my-zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,my-zenburn-red))))
   `(vertical-border ((t (:foreground ,my-zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,my-zenburn-fg :weight normal))))
   `(font-lock-comment-face ((t (:foreground ,my-zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,my-zenburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,my-zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,my-zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,my-zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,my-zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,my-zenburn-green :weight normal))))
   `(font-lock-string-face ((t (:foreground ,my-zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,my-zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,my-zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,my-zenburn-yellow-2 :weight normal))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,my-zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,my-zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,my-zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,my-zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,my-zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,my-zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,my-zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,my-zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,my-zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,my-zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,my-zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,my-zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,my-zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,my-zenburn-blue :weight normal))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,my-zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,my-zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,my-zenburn-bg-1 :foreground ,my-zenburn-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,my-zenburn-fg-1 :background ,my-zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,my-zenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(android-mode-info-face ((t (:foreground ,my-zenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,my-zenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,my-zenburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,my-zenburn-cyan :weight normal))))
;;;;; auctex
   `(font-latex-normal-face ((t (:inherit normal))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,my-zenburn-red :weight normal ))))
   `(font-latex-sedate-face ((t (:foreground ,my-zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,my-zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,my-zenburn-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,my-zenburn-bg+3 :foreground ,my-zenburn-bg-2))))
   `(ac-selection-face ((t (:background ,my-zenburn-blue-4 :foreground ,my-zenburn-fg))))
   `(popup-tip-face ((t (:background ,my-zenburn-yellow-2 :foreground ,my-zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,my-zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,my-zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,my-zenburn-bg :foreground ,my-zenburn-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,my-zenburn-yellow :background ,my-zenburn-bg))))
   `(company-tooltip-selection ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,my-zenburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,my-zenburn-yellow :background ,my-zenburn-bg-1))))
   `(company-tooltip-common-selection ((t (:background ,my-zenburn-bg-1))))
   `(company-scrollbar-fg ((t (:background ,my-zenburn-green+1))))
   `(company-scrollbar-bg ((t (:background ,my-zenburn-bg-1))))
   `(company-preview ((t (:background ,my-zenburn-green+1))))
   `(company-preview-common ((t (:background ,my-zenburn-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,my-zenburn-yellow-1 :foreground ,my-zenburn-bg))))
   `(bm-fringe-face ((t (:background ,my-zenburn-yellow-1 :foreground ,my-zenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,my-zenburn-green-1 :foreground ,my-zenburn-bg))))
   `(bm-persistent-face ((t (:background ,my-zenburn-green-1 :foreground ,my-zenburn-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,my-zenburn-orange :weight normal :underline t))))
   `(clojure-test-error-face ((t (:foreground ,my-zenburn-red :weight normal :underline t))))
   `(clojure-test-success-face ((t (:foreground ,my-zenburn-green+1 :weight normal :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,my-zenburn-blue :foreground ,my-zenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,my-zenburn-bg-05 :foreground ,my-zenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,my-zenburn-cyan :foreground ,my-zenburn-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,my-zenburn-green+4 :background nil))
                 (t (:foreground ,my-zenburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,my-zenburn-yellow))))
   `(diff-removed ((,class (:foreground ,my-zenburn-red :background nil))
                   (t (:foreground ,my-zenburn-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight normal)))
   `(diff-refine-change ((t :inherit diff-changed :weight normal)))
   `(diff-refine-removed ((t :inherit diff-removed :weight normal)))
   `(diff-header ((,class (:background ,my-zenburn-bg+2))
                  (t (:background ,my-zenburn-fg :foreground ,my-zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,my-zenburn-bg+2 :foreground ,my-zenburn-fg :normal t))
      (t (:background ,my-zenburn-fg :foreground ,my-zenburn-bg :normal t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,my-zenburn-blue-2 :background ,my-zenburn-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,my-zenburn-red+1 :background ,my-zenburn-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,my-zenburn-green+1 :background ,my-zenburn-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,my-zenburn-yellow :background ,my-zenburn-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,my-zenburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,my-zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,my-zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,my-zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,my-zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,my-zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,my-zenburn-blue :background ,my-zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,my-zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,my-zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,my-zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,my-zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,my-zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,my-zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,my-zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,my-zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,my-zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,my-zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,my-zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,my-zenburn-fg))))
   `(diredp-number ((t (:foreground ,my-zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,my-zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,my-zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,my-zenburn-green-1))))
   `(diredp-symlink ((t (:foreground ,my-zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,my-zenburn-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,my-zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,my-zenburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,my-zenburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,my-zenburn-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-red-2 :weight normal))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-red-2 weight normal))))
   `(ediff-fine-diff-B ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-green :weight normal))))
   `(ediff-fine-diff-C ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-blue-3 :weight normal ))))
   `(ediff-odd-diff-A ((t (:background ,my-zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,my-zenburn-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,my-zenburn-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,my-zenburn-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,my-zenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,my-zenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,my-zenburn-green+3))))
   `(egg-branch ((t (:foreground ,my-zenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,my-zenburn-yellow))))
   `(egg-term ((t (:foreground ,my-zenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,my-zenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,my-zenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,my-zenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,my-zenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,my-zenburn-green+4))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,my-zenburn-yellow :underline t
                                 :weight normal))))
   `(w3m-arrived-anchor ((t (:foreground ,my-zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,my-zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,my-zenburn-yellow
                                                     :underline t :weight normal))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,my-zenburn-bg-1
                                     :foreground ,my-zenburn-orange
                                     :weight normal))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,my-zenburn-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-normal-face ((t (:weight normal))))
   `(erc-current-nick-face ((t (:foreground ,my-zenburn-blue :weight normal))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,my-zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,my-zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,my-zenburn-blue :weight normal))))
   `(erc-nick-default-face ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(erc-my-nick-face ((t (:foreground ,my-zenburn-red :weight normal))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,my-zenburn-green))))
   `(erc-pal-face ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(erc-prompt-face ((t (:foreground ,my-zenburn-orange :background ,my-zenburn-bg :weight normal))))
   `(erc-timestamp-face ((t (:foreground ,my-zenburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,my-zenburn-green+4 :background ,my-zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,my-zenburn-red :background ,my-zenburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(eshell-ls-archive ((t (:foreground ,my-zenburn-red-1 :weight normal))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,my-zenburn-blue+1 :weight normal))))
   `(eshell-ls-executable ((t (:foreground ,my-zenburn-red+1 :weight normal))))
   `(eshell-ls-unreadable ((t (:foreground ,my-zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(eshell-ls-symlink ((t (:foreground ,my-zenburn-cyan :weight normal))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,my-zenburn-green+2 :weight normal))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-red-1) :inherit unspecified))
      (t (:foreground ,my-zenburn-red-1 :weight normal :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-yellow) :inherit unspecified))
      (t (:foreground ,my-zenburn-yellow :weight normal :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-cyan) :inherit unspecified))
      (t (:foreground ,my-zenburn-cyan :weight normal :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,my-zenburn-red-1 :weight normal))))
   `(flycheck-fringe-warning ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(flycheck-fringe-info ((t (:foreground ,my-zenburn-cyan :weight normal))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,my-zenburn-red-1 :weight normal :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,my-zenburn-orange :weight normal :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,my-zenburn-green-1 :weight normal :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-orange) :inherit unspecified))
      (t (:foreground ,my-zenburn-orange :weight normal :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-red) :inherit unspecified))
      (t (:foreground ,my-zenburn-red-1 :weight normal :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,my-zenburn-fg))))
   `(ack-file ((t (:foreground ,my-zenburn-blue))))
   `(ack-line ((t (:foreground ,my-zenburn-yellow))))
   `(ack-match ((t (:foreground ,my-zenburn-orange :background ,my-zenburn-bg-1 :weight normal))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,my-zenburn-green :weight normal :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,my-zenburn-red :weight normal :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,my-zenburn-magenta :weight normal :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,my-zenburn-fg :weight normal :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,my-zenburn-green  :weight normal))))
   `(git-gutter-fr:deleted ((t (:foreground ,my-zenburn-red :weight normal))))
   `(git-gutter-fr:modified ((t (:foreground ,my-zenburn-magenta :weight normal))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, my-zenburn-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:normal t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:normal t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:normal t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:normal t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:normal t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:normal t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:normal t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:normal t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:normal t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:normal t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:normal t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:normal t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:normal t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:normal t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,my-zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,my-zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,my-zenburn-green :weight normal))))
   `(gnus-summary-high-ticked ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(gnus-summary-high-unread ((t (:foreground ,my-zenburn-fg :weight normal))))
   `(gnus-summary-low-ancient ((t (:foreground ,my-zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,my-zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(gnus-summary-low-unread ((t (:foreground ,my-zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,my-zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,my-zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(gnus-summary-normal-unread ((t (:foreground ,my-zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(gnus-cite-1 ((t (:foreground ,my-zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,my-zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,my-zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,my-zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,my-zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,my-zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,my-zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,my-zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,my-zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,my-zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,my-zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,my-zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,my-zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,my-zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,my-zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,my-zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,my-zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,my-zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,my-zenburn-yellow))))
   `(gnus-x ((t (:background ,my-zenburn-fg :foreground ,my-zenburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,my-zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,my-zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,my-zenburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,my-zenburn-green
                      :background ,my-zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,my-zenburn-yellow
                      :background ,my-zenburn-bg-1
                      :underline nil
                      :weight normal
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,my-zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,my-zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,my-zenburn-green+4 :background ,my-zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,my-zenburn-red :background ,my-zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,my-zenburn-red :background ,my-zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,my-zenburn-orange :background ,my-zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,my-zenburn-magenta :background ,my-zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,my-zenburn-yellow :background ,my-zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,my-zenburn-magenta :background ,my-zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,my-zenburn-red :background ,my-zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,my-zenburn-cyan :background ,my-zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,my-zenburn-fg-1 :background ,my-zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,my-zenburn-cyan :background ,my-zenburn-bg :weight normal))))
   `(helm-ff-file ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,my-zenburn-red :background ,my-zenburn-bg :weight normal))))
   `(helm-ff-symlink ((t (:foreground ,my-zenburn-yellow :background ,my-zenburn-bg :weight normal))))
   `(helm-ff-prefix ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,my-zenburn-cyan :background ,my-zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,my-zenburn-fg-1 :background ,my-zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,my-zenburn-red :background ,my-zenburn-bg))))
   `(helm-moccur-buffer ((t (:foreground ,my-zenburn-cyan :background ,my-zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,my-zenburn-fg-1 :background ,my-zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,my-zenburn-bg-05))
                   (t :weight normal)))
   `(hl-line ((,class (:background ,my-zenburn-bg-05)) ; old emacsen
              (t :weight normal)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,my-zenburn-bg+1))
                   (t :weight normal)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(ido-only-match ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(ido-subdir ((t (:foreground ,my-zenburn-yellow))))
   `(ido-indicator ((t (:foreground ,my-zenburn-yellow :background ,my-zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,my-zenburn-bg+2 :weight normal))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,my-zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,my-zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,my-zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,my-zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,my-zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,my-zenburn-red+1))))
   `(jabber-activity-face((t (:foreground ,my-zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,my-zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight normal))))
   `(jabber-title-medium ((t (:height 1.2 :weight normal))))
   `(jabber-title-large ((t (:height 1.3 :weight normal))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,my-zenburn-orange))))
   `(js2-error ((t (:foreground ,my-zenburn-red :weight normal))))
   `(js2-jsdoc-tag ((t (:foreground ,my-zenburn-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,my-zenburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,my-zenburn-green+3))))
   `(js2-function-param ((t (:foreground, my-zenburn-green+3))))
   `(js2-external-variable ((t (:foreground ,my-zenburn-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,my-zenburn-red-1 :weight normal))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,my-zenburn-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,my-zenburn-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,my-zenburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,my-zenburn-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,my-zenburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,my-zenburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,my-zenburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,my-zenburn-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,my-zenburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,my-zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,my-zenburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,my-zenburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,my-zenburn-red-1 :weight normal))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,my-zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,my-zenburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,my-zenburn-green+2 :background ,my-zenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,my-zenburn-red+1 :background ,my-zenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,my-zenburn-blue+1 :background ,my-zenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,my-zenburn-magenta :background ,my-zenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,my-zenburn-yellow :background ,my-zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,my-zenburn-bg+1))))
   `(magit-section-title ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(magit-process-ok ((t (:foreground ,my-zenburn-green :weight normal))))
   `(magit-process-ng ((t (:foreground ,my-zenburn-red :weight normal))))
   `(magit-branch ((t (:foreground ,my-zenburn-blue :weight normal))))
   `(magit-log-author ((t (:foreground ,my-zenburn-orange))))
   `(magit-log-sha1 ((t (:foreground, my-zenburn-orange))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,my-zenburn-green+1))))
   `(message-header-other ((t (:foreground ,my-zenburn-green))))
   `(message-header-to ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(message-header-from ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(message-header-cc ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(message-header-newsgroups ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(message-header-subject ((t (:foreground ,my-zenburn-orange :weight normal))))
   `(message-header-xheader ((t (:foreground ,my-zenburn-green))))
   `(message-mml ((t (:foreground ,my-zenburn-yellow :weight normal))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,my-zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,my-zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,my-zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,my-zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,my-zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,my-zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,my-zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,my-zenburn-fg :weight normal))))
   `(mew-face-header-warning ((t (:foreground ,my-zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,my-zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,my-zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,my-zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,my-zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,my-zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,my-zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,my-zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,my-zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,my-zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,my-zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,my-zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,my-zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,my-zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,my-zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,my-zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,my-zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,my-zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,my-zenburn-cyan :background ,my-zenburn-bg :weight normal))))
   `(paren-face-mismatch ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-magenta :weight normal))))
   `(paren-face-no-match ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-red :weight normal))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,my-zenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,my-zenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,my-zenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,my-zenburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,my-zenburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,my-zenburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,my-zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,my-zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,my-zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,my-zenburn-red))))
   `(nav-face-file ((t (:foreground ,my-zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,my-zenburn-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,my-zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,my-zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,my-zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,my-zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,my-zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,my-zenburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,my-zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,my-zenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,my-zenburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,my-zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,my-zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,my-zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,my-zenburn-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,my-zenburn-fg+1 :slant italic :weight normal))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,my-zenburn-fg :weight normal))))
   `(org-checkbox ((t (:background ,my-zenburn-bg+2 :foreground ,my-zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,my-zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,my-zenburn-red-1))))
   `(org-done ((t (:normal t :weight normal :foreground ,my-zenburn-green+3))))
   `(org-formula ((t (:foreground ,my-zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,my-zenburn-green+3))))
   `(org-hide ((t (:foreground ,my-zenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,my-zenburn-orange))))
   `(org-level-2 ((t (:foreground ,my-zenburn-green+4))))
   `(org-level-3 ((t (:foreground ,my-zenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,my-zenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,my-zenburn-cyan))))
   `(org-level-6 ((t (:foreground ,my-zenburn-green+2))))
   `(org-level-7 ((t (:foreground ,my-zenburn-red-4))))
   `(org-level-8 ((t (:foreground ,my-zenburn-blue-4))))
   `(org-link ((t (:foreground ,my-zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,my-zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,my-zenburn-red))))
   `(org-scheduled-today ((t (:foreground ,my-zenburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,my-zenburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,my-zenburn-green+2))))
   `(org-tag ((t (:normal t :weight normal))))
   `(org-time-grid ((t (:foreground ,my-zenburn-orange))))
   `(org-todo ((t (:normal t :foreground ,my-zenburn-red :weight normal))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:normal t :foreground ,my-zenburn-red :weight normal :underline nil))))
   `(org-column ((t (:background ,my-zenburn-bg-1))))
   `(org-column-title ((t (:background ,my-zenburn-bg-1 :underline t :weight normal))))
   `(org-mode-line-clock ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-red-1))))
   `(org-ellipsis ((t (:foreground ,my-zenburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,my-zenburn-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,my-zenburn-orange))))
   `(outline-2 ((t (:foreground ,my-zenburn-green+4))))
   `(outline-3 ((t (:foreground ,my-zenburn-blue-1))))
   `(outline-4 ((t (:foreground ,my-zenburn-yellow-2))))
   `(outline-5 ((t (:foreground ,my-zenburn-cyan))))
   `(outline-6 ((t (:foreground ,my-zenburn-green+2))))
   `(outline-7 ((t (:foreground ,my-zenburn-red-4))))
   `(outline-8 ((t (:foreground ,my-zenburn-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,my-zenburn-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,my-zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,my-zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,my-zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,my-zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-orange))))
   `(proof-error-face ((t (:foreground ,my-zenburn-fg :background ,my-zenburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-orange))))
   `(proof-locked-face ((t (:background ,my-zenburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-orange))))
   `(proof-queue-face ((t (:background ,my-zenburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,my-zenburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,my-zenburn-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,my-zenburn-bg))))
   `(proof-warning-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,my-zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,my-zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,my-zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,my-zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,my-zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,my-zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,my-zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,my-zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,my-zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,my-zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,my-zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,my-zenburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,my-zenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,my-zenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,my-zenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,my-zenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,my-zenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,my-zenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,my-zenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,my-zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:normal t))))
   `(rcirc-prompt ((t (:foreground ,my-zenburn-yellow :normal t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:normal t))))
   `(rcirc-url ((t (:normal t))))
   `(rcirc-keyword ((t (:foreground ,my-zenburn-yellow :normal t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,my-zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,my-zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,my-zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,my-zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,my-zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,my-zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,my-zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,my-zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,my-zenburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,my-zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,my-zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,my-zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,my-zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,my-zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,my-zenburn-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,my-zenburn-yellow :normal t))))
   `(sh-quoted-exec ((t (:foreground ,my-zenburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,my-zenburn-red+1 :background ,my-zenburn-bg+3 :weight normal))))
   `(show-paren-match ((t (:background ,my-zenburn-bg+3 :weight normal))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,my-zenburn-red+1 :background ,my-zenburn-bg+3 :weight normal))))
   `(sp-show-pair-match-face ((t (:background ,my-zenburn-bg+3 :weight normal))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,my-zenburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,my-zenburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-red)))
      (t
       (:underline ,my-zenburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-orange)))
      (t
       (:underline ,my-zenburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-yellow)))
      (t
       (:underline ,my-zenburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,my-zenburn-green)))
      (t
       (:underline ,my-zenburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,my-zenburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,my-zenburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,my-zenburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,my-zenburn-red))))
   `(speedbar-separator-face ((t (:foreground ,my-zenburn-bg :background ,my-zenburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,my-zenburn-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,my-zenburn-fg
                                    :background ,my-zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,my-zenburn-fg
                                      :background ,my-zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,my-zenburn-fg
                                        :background ,my-zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,my-zenburn-bg
                                       :background ,my-zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,my-zenburn-red-2
                                       :background ,my-zenburn-red-4))))
   `(term-color-green ((t (:foreground ,my-zenburn-green
                                       :background ,my-zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,my-zenburn-orange
                                       :background ,my-zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,my-zenburn-blue-1
                                      :background ,my-zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,my-zenburn-magenta
                                         :background ,my-zenburn-red))))
   `(term-color-cyan ((t (:foreground ,my-zenburn-cyan
                                       :background ,my-zenburn-blue))))
   `(term-color-white ((t (:foreground ,my-zenburn-fg
                                       :background ,my-zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,my-zenburn-fg+1 :weight normal))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,my-zenburn-red-1 :weight normal))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,my-zenburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,my-zenburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,my-zenburn-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,my-zenburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,my-zenburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,my-zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,my-zenburn-green+3 :weight normal))))
   `(web-mode-css-rule-face ((t (:foreground ,my-zenburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,my-zenburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,my-zenburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,my-zenburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,my-zenburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,my-zenburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,my-zenburn-bg+1 :foreground ,my-zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,my-zenburn-bg+1 :foreground ,my-zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,my-zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,my-zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,my-zenburn-red))))
   `(whitespace-line ((t (:background ,my-zenburn-bg :foreground ,my-zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,my-zenburn-orange :foreground ,my-zenburn-orange))))
   `(whitespace-indentation ((t (:background ,my-zenburn-yellow :foreground ,my-zenburn-red))))
   `(whitespace-empty ((t (:background ,my-zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,my-zenburn-yellow :foreground ,my-zenburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,my-zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,my-zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,my-zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,my-zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,my-zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,my-zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,my-zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,my-zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,my-zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,my-zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,my-zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,my-zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,my-zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,my-zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,my-zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,my-zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,my-zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,my-zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,my-zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,my-zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,my-zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,my-zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,my-zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,my-zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,my-zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight normal))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,my-zenburn-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,my-zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,my-zenburn-bg-1 :foreground ,my-zenburn-bg-1))))
   ))

;;; Theme Variables
(my-zenburn-with-color-variables
  (custom-theme-set-variables
   'my-zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,my-zenburn-bg ,my-zenburn-red ,my-zenburn-green ,my-zenburn-yellow
                                          ,my-zenburn-blue ,my-zenburn-magenta ,my-zenburn-cyan ,my-zenburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,my-zenburn-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,my-zenburn-red-1)
       ( 40. . ,my-zenburn-red)
       ( 60. . ,my-zenburn-orange)
       ( 80. . ,my-zenburn-yellow-2)
       (100. . ,my-zenburn-yellow-1)
       (120. . ,my-zenburn-yellow)
       (140. . ,my-zenburn-green-1)
       (160. . ,my-zenburn-green)
       (180. . ,my-zenburn-green+1)
       (200. . ,my-zenburn-green+2)
       (220. . ,my-zenburn-green+3)
       (240. . ,my-zenburn-green+4)
       (260. . ,my-zenburn-cyan)
       (280. . ,my-zenburn-blue-2)
       (300. . ,my-zenburn-blue-1)
       (320. . ,my-zenburn-blue)
       (340. . ,my-zenburn-blue+1)
       (360. . ,my-zenburn-magenta)))
   `(vc-annotate-very-old-color ,my-zenburn-magenta)
   `(vc-annotate-background ,my-zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar my-zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for my-zenburn color names.
In buffers visiting library `my-zenburn-theme.el' the my-zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar my-zenburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after my-zenburn activate)
;;   "Maybe also add font-lock keywords for my-zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or my-zenburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "my-zenburn-theme.el")))
;;     (unless my-zenburn-colors-font-lock-keywords
;;       (setq my-zenburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car my-zenburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc my-zenburn-colors-alist))))))
;;     (font-lock-add-keywords nil my-zenburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after my-zenburn activate)
;;   "Also remove font-lock keywords for my-zenburn colors."
;;   (font-lock-remove-keywords nil my-zenburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'my-zenburn)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; my-zenburn-theme.el ends here
