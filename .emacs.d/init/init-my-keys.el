;; Normal keybindings that are overridden
(global-set-key (kbd "M-/")      'company-complete)
(global-set-key (kbd "C-a")      'smart-beginning-of-line)
(global-set-key (kbd "C-q")      'er/expand-region)
(global-set-key (kbd "C-x f")    'helm-find-files)
(global-set-key (kbd "C-x C-f")  'helm-find-files)
(global-set-key (kbd "C-x g")    'magit-status)
(global-set-key (kbd "C-x O")    'last-window)
(global-set-key (kbd "C-x C-O")  'last-window)
(global-set-key (kbd "C-x C-e")  'eval-replace-sexp)
(global-set-key (kbd "C-y")      'clipboard-yank)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "M-w")      'clipboard-kill-ring-save)
(global-set-key (kbd "M-z")      'ace-jump-zap-up-to-char)
(global-set-key [(meta g)]       'goto-line)
(global-set-key (kbd "<f1> f")   'counsel-describe-function)
(global-set-key (kbd "<f1> v")   'counsel-describe-variable)
(global-set-key [f5]             'eval-buffer)
(global-set-key [f6]             'rename-this-file-and-buffer)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Move by lines of five
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; Overview of the C-c prefix
;; C-c a * - Ag things
;; C-c e   - Eshell
;; C-c f * - File finding functions
;; C-c g * - Go places
;; C-c l * - Move in a lisp like fashion (by balanced expression)
;; C-c k * - Kill things structurally
;; C-c m * - Mu4e things
;; C-c o * - Org utilities
;; C-c s * - Substitute text
;; C-c v   - Version control for mercurial

(global-set-key (kbd "C-c q")   'mc/edit-lines)
(global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c a d") 'ag-dired-regexp)
(global-set-key (kbd "C-c a p") 'ag-project-regexp)
(global-set-key (kbd "C-c a r") 'ag-regexp)
(global-set-key (kbd "C-c c")   'compile)
(global-set-key (kbd "C-c e")   'eshell)
(global-set-key (kbd "C-c f g") 'counsel-git-grep)
(global-set-key (kbd "C-c f i") 'get-init-file)
(global-set-key (kbd "C-c f l") 'counsel-locate)
(global-set-key (kbd "C-c f n") 'toggle-tree)
(global-set-key (kbd "C-c f p") 'find-file-at-point)
(global-set-key (kbd "C-c g n") 'goto-line)
(global-set-key (kbd "C-c g o") 'occur)
(global-set-key (kbd "C-c k b") 'kill-braces)
(global-set-key (kbd "C-c k f") 'delete-frame)
(global-set-key (kbd "C-c k p") 'kill-parens)
(global-set-key (kbd "C-c l f") 'forward-sexp)
(global-set-key (kbd "C-c l j") 'backward-sexp)
(global-set-key (kbd "C-c l n") 'down-list)
(global-set-key (kbd "C-c l p") 'up-list)
(global-set-key (kbd "C-c n")   'highlight-symbol-next)
(global-set-key (kbd "C-c m i") 'jump-to-mailbox)
(global-set-key (kbd "C-c m u") 'mu4e-update-mail-and-index)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-switchb)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-o")     'ace-window)
(global-set-key (kbd "C-c z")   'ace-jump-mode)
(global-set-key (kbd "C-c ?")   'ace-jump-line-mode)
(global-set-key (kbd "C-c p")   'highlight-symbol-prev)

;; Functions for text manipulation
(defun qrc (replace-str)
   (interactive "sDo query-replace current word with: ")
   (forward-word)
   (let ((end (point)))
      (backward-word)
      (kill-ring-save (point) end)
      (query-replace (current-kill 0) replace-str) ))

(global-set-key (kbd "C-c r") 'qrc)

;; query replace all from buffer start
(fset 'my-query-replace-all 'query-replace)
(advice-add 'my-query-replace-all
            :around
            #'(lambda(oldfun &rest args)
               "Query replace the whole buffer."
               ;; set start pos
               (unless (nth 3 args)
                 (setf (nth 3 args)
                       (if (region-active-p)
                           (region-beginning)
                         (point-min))))
               (unless (nth 4 args)
                 (setf (nth 4 args)
                       (if (region-active-p)
                           (region-end)
                         (point-max))))
               (apply oldfun args)))
(global-set-key "\C-cR" 'my-query-replace-all)

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
 (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key [C-backspace] #'er-switch-to-previous-buffer)

;; Terminate shell process when line is empty
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(provide 'init-my-keys)
