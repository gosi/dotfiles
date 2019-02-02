(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; Require the company backends I like. This goes here so it can be
;; turned on and off all at once.
(require-package 'slime-company)
(require-package 'company-math)
(require-package 'company-auctex)
(require-package 'company-jedi)
(company-auctex-init)

(provide 'init-company)
