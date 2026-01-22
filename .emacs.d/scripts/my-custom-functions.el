
;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun new-line-above ()
  "Add an empty line above and move the cursor to this line."
  (interactive)
  (back-to-indentation)
  (split-line))

(defun new-line-below ()
  "Add an empty line below and move the cursor tol to this line"
  (interactive)
  (end-of-visual-line)
  (newline-and-indent))

(provide 'my-custom-functions)
