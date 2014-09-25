;;; sonic-console.el --- Object inspector -*- lexical-binding: t -*-

(require 'json)

(defconst sonic-message-buffer-name "*sonicpi-messages*")
(defcustom sonic-log-messages t
  "If non-nil, log protocol messages to the `sonic-message-buffer-name' buffer."
  :type 'boolean
  :group 'sonic)

(defconst sonic-message-buffer-max-size 1000000)
(defconst sonic-message-buffer-reduce-denominator 4)

(defun sonic-messages-buffer ()
  "Return or create the buffer given by `sonic-message-buffer-name'.
The default buffer name is *nrepl-messages*."
  (or (get-buffer sonic-message-buffer-name)
      (let ((buffer (get-buffer-create sonic-message-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (setq-local comment-start ";")
          (setq-local comment-end ""))
        buffer)))

(defun sonic-log-message (msg)
  "Log the given MSG to the buffer given by `sonic-message-buffer-name'."
  (when sonic-log-messages
    (with-current-buffer (sonic-messages-buffer)
      (when (> (buffer-size) sonic-message-buffer-max-size)
        (goto-char (/ (buffer-size) sonic-message-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (sonic--pp msg)
      (-when-let (win (get-buffer-window))
        (set-window-point win (point-max))))))

(defun sonic--pp (object)
  (insert (format "π> %s\n" object)))

(provide 'sonic-console)

;;; sonic-console.el ends here
