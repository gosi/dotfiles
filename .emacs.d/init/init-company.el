(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; Require the company backends I like. This goes here so it can be
;; turned on and off all at once.
(require-package 'slime-company)
(require-package 'company-math)
(require-package 'company-auctex)
(require-package 'company-jedi)
(company-auctex-init)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(provide 'init-company)
