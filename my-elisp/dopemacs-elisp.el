;;; Comentary: 



;;; Code:


(defun dopemacs-prueba ()
  (interactive)
  (let ((p (point)))
    (indent-according-to-mode)
    (when (and (= p (point))
               (not (bolp))
               (looking-at "\\_>"))
      (hippie-expand nil)))
  )


(defun dopemacs-meta-tab (miarg)
  (interactive)
  (apply (key-binding "\C-[\C-i") miarg)
  )

(defun dopemacs-pegar-linea ()
  (interactive)
  (move-beginning-of-line nil)
  (yank)
  )

(defun dopemacs-kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

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

;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active ank(list (region-beginning) (region-end))
;;   (list (line-beginning-position)
;;         (line-beginning-position 2)))))

;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;   (list (line-beginning-position)
;;         (line-beginning-position 2)))))


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






(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (save-excursion
      (set-buffer bf)
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;;(move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer)))))

(defun gud-kill-buffer ()
  (if (eq major-mode 'gud-mode)
      (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)



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
  "Switch buffers with no questions asked"
  (interactive)
  (switch-to-buffer nil t))


(defun dopemacs-toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  )

(defun dopemacs-validar-xml ()
  (interactive)
  (compile  (concat "xmllint --noout --schema " (ido-read-file-name "XSD: ") " " (buffer-file-name)))
  )

(defun dopemacs-generar-tags ()
  (interactive)
  (shell-command  (concat "cd " (ido-read-directory-name "Root dir: ") " && ctags-exuberant -R -e " ))
  )

(defun dopemacs-formatear-xml ()
  (interactive)
  (shell-command-on-region (point-min)(point-max) "xmllint --format -" (current-buffer) t)
  )


(defun dopemacs-file-name-without-tramp (filename)
  (if (file-remote-p filename)
      (tramp-file-name-localname (tramp-dissect-file-name filename))
    (filename)))

(defun dopemacs-jshint ()
  (interactive)
  (let ((filename (dopemacs-file-name-without-tramp(buffer-file-name))))
    (compile (concat "cd " temporary-file-directory " && jshint --extract=always " filename))))



(defun dopemacs-md5 ()
  (interactive)
  (shell-command (concat "md5sum " (thing-at-point 'filename)))
  )

(defun dopemacs-unir-lineas ()
  (interactive)
  (next-line)
  (join-line)
  )


(defun dopemacs-copiar-linea (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  )

(defun dopemacs-codesearch-at-point ()
  "Searches current function in Google Code Search"
  (interactive)
  (browse-url (concat "http://www.google.com/codesearch?q=" (current-word)))
  )

(defun dopemacs-pidof (process)
  (interactive "sProcess: ")
  (shell-command (concat "pidof " process) t)
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

(defun dopemacs-pdb-breakpoint ()
  (interactive)
  (insert "import pdb; pdb.set_trace()")
  )

(defun dopemacs-pdb-aliases ()
  (interactive)
  (insert "alias pi for key,value in %1.__dict__.items(): print \"%1.%s = %s\" % (key,value)")
  )

(defun dopemacs-mi-split-window (window)
  (if (eq (buffer-name (window-buffer (window))) "*compilation*")
      (with-selected-window window (split-window-vertically))
    (with-selected-window window (split-window-sensibly))
    )
  )


(defun dopemacs-grunt ()
  "Run grunt"
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
         (result (call-process-shell-command "grunt" nil grunt-buffer t))
         (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) grunt-buffer)))))


                                        ; save the current macro as reusable function.
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



(defun dopemacs-kill-this-buffer () 
  (interactive) 
  (kill-buffer (current-buffer)))
                                        ; colored shell commands via C-!
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun dopemacs-babcore-shell-execute(cmd)
  "Execute a shell command in an interactive shell buffer."
  (interactive "sShell command: ")
  (shell (get-buffer-create "*shell-commands-buf*"))
  (process-send-string (get-buffer-process "*shell-commands-buf*") (concat cmd "\n")))






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


(defun dopemacs-backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(provide 'dopemacs-elisp)
;;; dopemacs-elisp.el ends here
