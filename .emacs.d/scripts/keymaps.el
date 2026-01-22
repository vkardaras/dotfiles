
;;; Keymaps

;; Configure which key
(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.3
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

;; Global keymaps
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(keymap-global-set "C-." 'completion-at-point)
(keymap-global-set "C-o" 'new-line-below)
(keymap-global-set "C-S-o" 'new-line-above)

;;; Git programs
;; (use-package git-timemachine
;;   :after git-timemachine
;;   :hook (evil-normalize-keymaps . git-timemachine-hook)
;;   :config
;;     (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
;;     (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
;; )
(use-package magit)

;;; Key Bindings

;; Define a command
(defun test-command ()
  (interactive)
  (message "this is a test"))

(defun open-init-file ()
  "emacs init file"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun open-emacs-dir ()
  "open emacs directory"
  (interactive)
  (dired user-emacs-directory))

;; Define a sub-keymap
(defvar-keymap vk-prefix-file-map
  :doc "Switch to another buffer, or bookmarked file, or recently opened file"
  "c" 'open-init-file
  "d" 'find-grep-dired
  "e" 'open-emacs-dir
  "f" 'find-file
  "g" 'consult-grep
  "l" 'consult-line
  "o" 'consult-outline
  "r" 'consult-recent-file
  "u" 'sudo-edit-find-file
  "U" 'sudo-edit)

(defvar-keymap vk-prefix-buffer-map
  :doc "My prefix map for bookmarks and buffers"
  "b" 'consult-buffer
  "c" 'clone-indirect-buffer
  "C" 'clone-indirect-buffer-other-window
  "d" 'bookmark-delete
  "i" 'ibuffer
  "k" 'kill-current-buffer
  "K" 'kill-some-buffers
  "l" 'list-bookmarks
  "m" 'bookmark-set
  "n" 'next-buffer
  "o" 'consult-outline
  "p" 'previous-buffer
  "r" 'revert-buffer
  "R" 'rename-buffer
  "s" 'basic-save-buffer
  "S" 'save-some-buffers
  "w" 'bookmark-save)

(defvar-keymap vk-prefix-window-map
  :doc "My prefix map for windows and words"
  "c" 'delete-window
  "s" 'split-window-below
  "v" 'split-window-right
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "w" 'other-window
  "d" 'downcase-word
  "u" 'upcase-word
  "=" 'count-words)

(defvar-keymap vk-prefix-toggle-map
  :doc "My prefix map for toggle various windows"
  "v" 'vterm-toggle
  "t" 'treemacs)
  ;;"t" 'test-command)

;; Define a keymap
(defvar-keymap my-test-prefix-map
  :doc "My prefix map"
  "." 'find-file
  ;; "=" 'perspective-map
  "TAB TAB" 'comment-line
  "u" 'universal-argument
  "b" vk-prefix-buffer-map
  "f" vk-prefix-file-map
  "w" vk-prefix-window-map
  "d" 'dired
  "h" help-map
  "t" vk-prefix-toggle-map)

;; Define a key binding
;;(with-eval-after-load 'flyspell
;;  (keymap-set global-map "C-;" nil))
;;(keymap-set global-map "C-;" my-test-prefix-map)

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil))
(global-set-key (kbd "C-;") my-test-prefix-map)

;; Set value for sub-keymap in which key
(which-key-add-keymap-based-replacements my-test-prefix-map
  "b" `("Buffer" . ,vk-prefix-buffer-map)
  "f" `("File" . ,vk-prefix-file-map)
  "w" `("Window" . ,vk-prefix-window-map)
  "h" `("Help" . ,help-map))

(provide 'keymaps.el)
