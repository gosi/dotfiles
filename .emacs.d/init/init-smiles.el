;; Sometimes you just need something cute to cheer you up :).
(defun get-next-cute ()
  "Grab the first /r/aww url from the aww page."
  (interactive)
  (let ((new-buffer (find-file "~/Downloads/.aww"))
        (image-url nil))
    (with-current-buffer new-buffer
      (while (not (looking-at "[a-zA-Z0-9]*[0-9][a-zA-Z0-9]*"))
        (search-forward-regexp "href=\"/r/aww/"))
      (setq image-url (strip-text-properties (match-string 0))))
    (kill-buffer new-buffer)
    (let ((full-url (concat "http://www.imgur.com/r/aww/" image-url)))
      (when (called-interactively-p) (message "%s" full-url))
      full-url)))

(defun cheer-me-up ()
  "Open a random cute picture."
  (interactive)
  (save-excursion
    (shell-command "wget -q http://www.imgur.com/r/aww -r -q -O ~/Downloads/.aww")
    (browse-url (get-next-cute))
    (delete-file "~/Downloads/.aww")
    (message "")))

(provide 'init-smiles)
