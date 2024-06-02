

;; 一些有用的扩展包
;; 搜索三剑客：ivy, counsel,swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(provide 'init-managing)