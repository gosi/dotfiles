;; Depending on elpy for all my python needs
(require-package 'elpy)

(package-initialize)
(elpy-enable)

;; Let's use jupyter for interactive python
(setq python-shell-interpreter "jupyter"
            python-shell-interpreter-args "console --simple-prompt"
                  python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
                          "jupyter")

(provide 'init-python)
