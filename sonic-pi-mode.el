;;; sonic-pi-mode.el --- Minor mode for SonicPi interactions -*- lexical-binding: t -*-

(require 'sonic-pi-osc)

(defun sonic-pi-quit ()
  "Shutdown SonicPi"
  (interactive)
  (sonic-pi-osc-cleanup)
  (sonic-pi-messages-buffer-cleanup)
  (sonic-pi-sonic-server-cleanup)
  )

(defvar sonic-pi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'sonic-pi-send-buffer)
    (define-key map (kbd "C-c C-q") 'sonic-pi-quit)
    map))

;;;###autoload
(define-minor-mode sonic-pi-mode
  "Minor mode for SonicPi interaction from a Ruby buffer."
  :lighter "π"
  :keymap sonic-pi-mode-map)

(provide 'sonic-pi-mode)

;;; sonic-pi-mode.el ends here
