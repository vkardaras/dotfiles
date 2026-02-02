;;; Programming

;; Project management
(use-package project
  :bind-keymap
  (("C-c p" . project-prefix-map)))

;; LSP
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . my/lsp-mode-setup-completion)
  ))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package lsp-ui)
(use-package consult-lsp)

;; enable formatting on save
(add-hook 'java-mode-hook #'lsp)
(setq lsp-enable-on-type-formatting t)
(setq lsp-java-format-on-type-enabled t)
;; specify Google style
(setq lsp-java-format-settings-url
      "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
(setq lsp-java-format-settings-profile "GoogleStyle")

;; DAP
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java
  :ensure nil
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue))
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(use-package yasnippet :config (yas-global-mode))

;; Magit
(use-package magit)

(use-package eglot
  :config
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:inlayHintProvider)
        eglot-confirm-server-initiated-edits nil))

(use-package rustic
  :config
  ; Tell rustic where to find the cargo binary
  (setq rustic-cargo-bin-remote "/usr/local/cargo/bin/cargo")
  (setq rustic-lsp-client 'eglot))

(provide 'programming)
