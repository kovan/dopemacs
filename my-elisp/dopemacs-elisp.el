;;; package --- Summary:
;; Lisp code for Dopemacs

;;; Commentary: 
;; Lisp code for Dopemacs


;;; Code:

(defun dopemacs-bug-report ()
    "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs version."
	  (interactive)
	    (browse-url "https://github.com/kovan/dopemacs/issues/new"))

(defun dopemacs-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun dopemacs-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun dopemacs-xah-cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

(defun dopemacs-xah-copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )


(defun dopemacs-toggle-window-split ()
  (interactive)
  (if (>= (count-windows) 2)
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

(defun dopemacs-switch-to-buffer-quick ()
  "Switch buffers with no questions asked."
  (interactive)
  (switch-to-buffer nil t))


(defun dopemacs-toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  )

(defun dopemacs-validate-xml ()
  (interactive)
  (compile  (concat "xmllint --noout --schema " (ido-read-file-name "XSD: ") " " (buffer-file-name)))
  )

(defun dopemacs-generate-tags ()
  (interactive)
  (shell-command  (concat "cd " (ido-read-directory-name "Root dir: ") " && ctags-exuberant -R -e " ))
  )

(defun dopemacs-format-xml ()
  (interactive)
  (shell-command-on-region (point-min)(point-max) "xmllint --format -" (current-buffer) t)
  )


(defun dopemacs-recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))



(defun dopemacs-nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (condition-case nil
                   (progn
                     (nxml-backward-up-element) ; always returns nil
                     t)
                 (error nil))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (setq header-line-format (format "/%s" (mapconcat 'identity path "/")))))))

(defun dopemacs-pudb-breakpoint ()
  (interactive)
  (insert "import pudb; pudb.set_trace()"))

(defun dopemacs-ipdb-breakpoint ()
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun dopemacs-pdb-aliases ()
  (interactive)
  (insert "alias pi for key,value in %1.__dict__.items(): print \"%1.%s = %s\" % (key,value)"))


(defun dopemacs-save-current-kbd-macro-to-dot-emacs (name)
  "Save the current macro as named function definition inside
your initialization file so you can reuse it anytime in the
future."
  (interactive "SSave Macro as: ")
  (name-last-kbd-macro name)
  (save-excursion
    (find-file-literally user-init-file)
    (goto-char (point-max))
    (insert "\n\n;; Saved macro\n")
    (insert-kbd-macro name)
    (insert "\n")))



(defun rgc-find-git-root ()
  "Find git root directory from current directory."
  (interactive)
  (rgc-member-directory default-directory
                        "~/"
                        (lambda (x)
                          (file-exists-p (concat x ".git")))))

(defun rgc-member-directory (from to fun &optional if-nil)
  "Returns a directory between `from' and `to' for wich `fun'
returns non nil. The search begins on the child 'from' and goes
up till 'to', or '/'. If `if-nil' is provided, in case of not
finding any suitable directory, it returns it instead of `to'"
  (when (not (file-exists-p from))
    (return))
  (if (or (equal (expand-file-name from) (expand-file-name to))
          (equal from "/")) ;how to do it multiplatform?
      (or if-nil to)
    (if (funcall fun from) from
      (rgc-member-directory (expand-file-name (concat from "/../")) ;how to do it multiplatform?
                            to
                            fun
                            if-nil))))

(defun dopemacs-find-file-in-repo ()
  (interactive)
  (let* ((git-root (rgc-find-git-root))
         (ido-enable-regexp nil)
         (repo-files (split-string
                      (with-temp-buffer
                        (cd git-root)
                        (shell-command "git ls-files" t)
                        (buffer-string)))))
    (find-file (concat git-root "/"
                       (ido-completing-read "File in repo: " repo-files t t)))))


(defun dopemacs-split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'dopemacs-split-window)
      (progn
        (jump-to-register :dopemacs-split-window)
        (setq this-command 'dopemacs-unsplit-window))
    (window-configuration-to-register :dopemacs-split-window)
    (switch-to-buffer-other-window nil)))


(defun dopemacs-fixed-width-font ()
	(interactive)
	(set (make-local-variable 'face-remapping-alist)
                   '((default :family "DejaVu Sans Mono")))
	)

(provide 'dopemacs-elisp)
;;; dopemacs-elisp.el ends here
