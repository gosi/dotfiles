;; C++ settings
(add-hook 'c++-mode-hook (lambda ()
    (define-key c++-mode-map [tab] 'clang-format-buffer)
    (setq company-clang-arguments '("-std=c++11"))
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-cppcheck-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11")))

(provide 'init-c-lang)
