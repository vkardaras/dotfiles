;;; Programming

;; Project management
;; (use-package project
;;   :bind-keymap
;;   (("C-c p" . project-prefix-map)))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config 
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)   
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))

;; LSP
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . my/lsp-mode-setup-completion)
  ))
;; (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
;; (use-package company)
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

;; DAP
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java
  :ensure nil
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue))

(use-package lsp-treemacs)

;; (add-hook 'compilation-filter-hook
;;           (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;; (use-package consult-lsp)

;; ;; enable formatting on save
;; (add-hook 'java-mode-hook #'lsp)
;; (setq lsp-enable-on-type-formatting t)
;; (setq lsp-java-format-on-type-enabled t)
;; ;; specify Google style
;; (setq lsp-java-format-settings-url
;;       "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
;; (setq lsp-java-format-settings-profile "GoogleStyle")

;; Magit
(use-package magit)

(provide 'programming)
