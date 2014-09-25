;;;###autoload
(defcustom sonic-mode-line
  '(:eval (format " sonic[%s]" (sonic-current-ns)))
  "Mode line ligher for `sonic-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `sonic-mode' displays its
status in the mode line.  The default value displays the current ns.
Set this variable to nil to disable the mode line
entirely."
  :group 'sonic
  :type 'sexp
  :risky t
  :package-version '(sonic "0.1.0"))

(defvar sonic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'sonic-send-buffer)
;;    (define-key map (kbd "C-c C-q") 'sonic-cleanup)
    ))

;;;###autoload
(define-minor-mode sonic-mode
  "Minor mode for SonicPi interaction from a Ruby buffer.

\\{sonic-mode-map}"
  nil
  sonic-mode-line
  sonic-mode-map
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'sonic-complete-at-point))

(provide 'sonic-mode)
