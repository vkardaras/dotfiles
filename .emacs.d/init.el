;;; init.el --- Vasilis init file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Vasilis' Emacs configuration
;;
;;; Code:

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; Set up the package manager

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
;;; Basic behaviour
;; Tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)    ;; You can select text and delete it by typing.

(electric-pair-mode 1)                   ; Turns on automatic parens pairing
(global-auto-revert-mode t)              ; Automatically show changes if the file has changed
(recentf-mode 1)
;; Save what you enter into minibuffer prompts
(setq history-length 10)
(savehist-mode 1)
;; Sane defaults
(save-place-mode 1)
(global-display-line-numbers-mode 1)     ; Display line numbers
(setq display-line-numbers-type 'relative)
(dolist (mode '(vterm-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(global-visual-line-mode t)              ; Enable truncated lines
(setq inhibit-startup-screen t)
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

;; Search and Replace
;; Display a counter showing the number of the current and the other
;; matches.  Place it before the prompt, though it can be after it.
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;; Make regular Isearch interpret the empty space as a regular
;; expression that matches any character between the words you give
;; it.
(setq search-whitespace-regexp ".*?")

;; Fonts
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
(setq-default line-spacing 0.12)

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm-loading))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package treemacs-nerd-icons
  :config
  (setq treemacs-project-follow-mode t)
  (treemacs-nerd-icons-config))

;;; Configure the minibuffer and completions 

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))
  ;; (setq completion-category-defaults nil)
  ;; (setq completion-category-overrides nil))

(use-package consult)

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult)

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.5))
  (corfu-count 14)
  (corfu-scroll-margin 4)
  ;; Have Corfu wrap around when going up
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-quit-at-boundary 'separator)
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  ;; Allow Corfu to show help text next to suggested completion
  (corfu-popupinfo-mode)
  :config
  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local corfu-auto nil
					    corfu-quit-at-boundary t
					    corfu-quit-no-match t)
				(corfu-mode))))

(use-package cape
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))


;;; The file manager (Dired)

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-guess-shell-alist-user
      '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh *" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv *" "xdg-open")
		(".*" "xdg-open"))))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil)
  ;; Refresh icons when inserting/removing subtree
  (defun my/nerd-icons-dired-refresh-after-subtree (&rest _)
    "Refresh `nerd-icons-dired` after expanding or collapsing subtrees."
    (when nerd-icons-dired-mode
      (nerd-icons-dired--refresh)))

  (advice-add 'dired-subtree-insert :after #'my/nerd-icons-dired-refresh-after-subtree)
  (advice-add 'dired-subtree-remove :after #'my/nerd-icons-dired-refresh-after-subtree))

(use-package dired-preview)

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Org Mode
(require 'org-tempo)
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(add-hook 'org-mode-hook 'org-indent-mode)

;; eneble syntax highlighting
(setq org-src-fontify-natively t)

;;; Spelling
;; flyspell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; harper-ls
;; (use-package eglot
;;   :hook
;;   (text-mode . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(text-mode . ("harper-ls" "--stdio"))))

;; Vterm
(use-package vterm
  :ensure t
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

;; Terraform
(use-package terraform-mode
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package yaml-mode)


(add-to-list 'load-path "~/.emacs.d/scripts/")

(require 'programming)
(require 'my-custom-functions)
(require 'keymaps)
