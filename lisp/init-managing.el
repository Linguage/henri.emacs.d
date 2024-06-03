;; 启用的扩展有：
;; - ivy
;; - counsel
;; - swiper
;; - which-key

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


;; NeoTree的相关设置
(use-package neotree
  :ensure t
  :defer t
  :config
  ; (setq neo-theme (if (display-graphic-p) 'nerd 'arrow)); 设置 NeoTree 主题为 icons 或 arrow
  ) 
(global-set-key (kbd "<f8>") 'neotree)
(provide 'init-managing)