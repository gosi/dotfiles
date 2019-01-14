;;Installing Counsel will bring in Ivy and Swiper as dependencies.
(require 'counsel)
(ivy-mode 1)

(use-package ivy :demand
   :config
   (setq ivy-use-virtual-buffers t
   ivy-count-format "%d/%d "))

(provide 'init-ivy)
