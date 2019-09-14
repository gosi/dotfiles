(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-support-shift-select 'always)

(setq org-log-done 'time)

(setq org-directory "~/org")
;; Capture for taking notes
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (concat org-directory "/agenda-files.org"))

(setq org-capture-templates
      '(("a" "Appointment" entry (file+datetree "~/org/appointments.org")
         "* %?\n %i\n %a")
        ("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n %i\n %a")))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


(provide 'init-org)
