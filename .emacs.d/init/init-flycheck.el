(require 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-to-list 'flycheck-checkers 'proselint)
(setq-default flycheck-highlighting-mode 'lines)

(provide 'init-flycheck)
