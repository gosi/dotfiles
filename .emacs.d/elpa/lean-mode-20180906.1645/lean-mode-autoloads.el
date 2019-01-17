;;; lean-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lean-info" "lean-info.el" (23616 32136 332386
;;;;;;  44000))
;;; Generated autoloads from lean-info.el

(autoload 'lean-info-mode "lean-info" "\
Major mode for Lean Info Buffer

\(fn)" t nil)

;;;***

;;;### (autoloads nil "lean-mode" "lean-mode.el" (23616 32136 288386
;;;;;;  355000))
;;; Generated autoloads from lean-mode.el

(autoload 'lean-mode "lean-mode" "\
Major mode for Lean
     \\{lean-mode-map}
Invokes `lean-mode-hook'.

\(fn)" t nil)

(push '("\\.lean$" . lean-mode) auto-mode-alist)

;;;***

;;;### (autoloads nil nil ("lean-debug.el" "lean-dev.el" "lean-eri.el"
;;;;;;  "lean-flycheck.el" "lean-hole.el" "lean-input.el" "lean-leanpkg.el"
;;;;;;  "lean-message-boxes.el" "lean-mode-pkg.el" "lean-right-click.el"
;;;;;;  "lean-server.el" "lean-settings.el" "lean-syntax.el" "lean-type.el"
;;;;;;  "lean-util.el") (23616 32136 352385 902000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lean-mode-autoloads.el ends here
