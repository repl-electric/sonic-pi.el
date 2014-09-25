;;; sonic-mode.el --- Object inspector -*- lexical-binding: t -*-

(defun sonic-quit ()
  "Shutdown SonicPi"
  (interactive)
  (sonic-cleanup))

(defvar sonic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'sonic-send-buffer)
    (define-key map (kbd "C-c C-q") 'sonic-quit)
    map))

;;;###autoload
(define-minor-mode sonic-mode
  "Minor mode for SonicPi interaction from a Ruby buffer."
  :lighter "π"
  :keymap sonic-mode-map)

(provide 'sonic-mode)

;;; sonic-mode.el ends here
