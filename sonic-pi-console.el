;;; sonic-pi-console.el --- Message buffer handling -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ansi-color)
(require 'dash)

(defconst sonic-pi-message-buffer-name "*sonic-pi-messages*")
(defcustom sonic-pi-log-messages t
  "If non-nil, log protocol messages to the `sonic-pi-message-buffer-name' buffer."
  :type 'boolean
  :group 'sonic)

(defconst sonic-pi-message-buffer-max-size 1000000)
(defconst sonic-pi-message-buffer-reduce-denominator 4)

(defconst sonic-pi-message-buffer-intro
  "Welcome to SonicPi http://sonic-pi.net, Audible computing.
-=π   -=π=   -=π   -=π
")

(defun sonic-pi-messages-buffer-init ()
  (let ((origin (current-buffer))
        (already-exists (get-buffer sonic-pi-message-buffer-name)))
    (select-window (display-buffer (sonic-pi-messages-buffer)))
    (if (not already-exists) (insert sonic-pi-message-buffer-intro))
    (select-window (display-buffer origin))))

(defun sonic-pi-messages-buffer-cleanup ()
  (when (get-buffer (sonic-pi-messages-buffer))
    (kill-buffer (sonic-pi-messages-buffer))))

(defun sonic-pi-messages-buffer ()
  "Return or create the buffer given by `sonic-pi-message-buffer-name'.
The default buffer name is *sonic-pi-messages*."
  (or (get-buffer sonic-pi-message-buffer-name)
      (let ((buffer (get-buffer-create sonic-pi-message-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (setq-local comment-start ";")
          (setq-local comment-end ""))
        buffer)))

(defun sonic-pi-log-message (level msg)
  "Log the given MSG to the buffer given by `sonic-pi-message-buffer-name'."
  (when sonic-pi-log-messages
    (with-current-buffer (sonic-pi-messages-buffer)
      (when (> (buffer-size) sonic-pi-message-buffer-max-size)
        (goto-char (/ (buffer-size) sonic-pi-message-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (sonic-pi--pp level msg)
      (-when-let (win (get-buffer-window))
        (set-window-point win (point-max))))))

(defun sonic-pi--pp (level object)
  (cl-flet ((error-color  (str) (propertize str 'face `(:weight ultra-bold :foreground , "red")))
            (sample-color (str) (propertize str 'face `(:weight bold :foreground , "pink")))
            (info-color   (str) (propertize str 'face `(:weight ultra-bold :foreground , "orange"))))
    (cond
     ((string-match "\/info*"  level) (progn
                                        (insert "π> ")
                                        (insert (info-color (format "%s\n" (car object))))))
     ((string-match "\/error*" level) (insert (error-color (format "π> Error: %s\n" object))))
     ((string-match "\/multi_message*" level)
      ;;TODO: multi_message does not batch messages together,
      ;;so we get them individual without msg-count being incremented.
      ;;Means we duplicate the Run information per message
      (progn
        (let ((job-id (first object))
              (thread-name (second object))
              (run-time (third object))
              (msg-count (fourth object)))
          (progn
            (insert (format "[Run %s, Time %s" job-id run-time))
            (if (not (string= "" thread-name))
                (insert (format ", Thread %s" thread-name)))
            (insert "]\n")
            (insert " └─ "))
          (insert (sample-color (format "%s\n" (first (last object))))))))
     (t (insert (format "π> %s %s\n" level object))))))

(provide 'sonic-pi-console)

;;; sonic-pi-console.el ends here