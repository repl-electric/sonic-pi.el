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
(defconst sonic-pi-mid-str   "   ├─ ")
(defconst sonic-pi-end-str   "   └─ ")
(defconst sonic-pi-start-str "   │ ")

(defconst sonic-pi-ignore-cues 1)

(defconst sonic-pi-message-buffer-intro
  "Welcome to SonicPi http://sonic-pi.net, Audible computing.
-=π   -=π  -=π   -=π
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
  (cl-flet ((text-color   (str) (propertize str 'face `(:weight normal     :foreground , "white")))
            (error-color  (str) (propertize str 'face `(:weight ultra-bold :background , "red")))
            (thread-color (str) (propertize str 'face `(:weight ultra-bold :foreground , "green")))
            (stdout-color (str) (propertize str 'face `(:weight ultra-bold :foreground , "orange")))
            (sample-color (str) (propertize str 'face `(:weight ultra-bold :foreground , "blue")))
            (info-color   (str) (propertize str 'face `(:weight normal     :foreground , "yellow"))))
    (cond
     ((string-match "\/info*"  level) (progn
                                        (insert "π> ")
                                        (insert (info-color (format "%s\n" (last object))))
                                        ))

     ((string-match "\/syntax_error" level)
     ((progn )
       (message (format "Error: %s" (second object)))
       (insert (error-color (format "π> Syntax Error: %s\n" (second object))))
     ))
     ((string-match "\/error" level)
      (progn
        (save-match-data ; is usually a good idea
          (and (string-match "\\([0-9]+\\)" (second object))
               (setq line-error (format "line-> [%s]" (match-string 1 (second object))))))
        (message (format "   Error: %s" line-error))
        (insert (error-color (replace-regexp-in-string
                              "&#39" "'"
                              (replace-regexp-in-string "&gt;" ">"
                                                        (format "π> Error: %s\n" (second object))))))))

     ((string-match "\/multi_message*" level)
      ;;TODO: multi_message does not batch messages together,
      ;;so we get them individual without msg-count being incremented.
      ;;Means we duplicate the Run information per message
      (progn
        (let ((job-id (first object))
              (thread-name (second object))
              (run-time (third object))
              (msg-count (fourth object))
              (data (nthcdr 4 object)))
          (when (> msg-count 1)
            (progn
              (insert "[")
              (if (not (string= "" thread-name))
                  (insert (format "%s" (thread-color thread-name))))
              (insert "]\n")
              (cl-loop for msg-type in data by (-partial 'nthcdr 2)
                       for msg-data in (cdr data) by (-partial 'nthcdr 2)
                       for idx from 0 to msg-count
                       do
                       (let ((format-s (if (and (= idx 1) (> msg-count 2))
                                           sonic-pi-start-str
                                         (if (or (= msg-count 2)
                                                 (= idx (+ 1 (/ msg-count 2))))
                                             sonic-pi-end-str
                                           sonic-pi-mid-str))))
                         (progn
                           (when (and (= msg-type 4) (= sonic-pi-ignore-cues 0))
                             (progn (insert (sample-color (format "%s %s\n" format-s msg-data)))))
                           (when (= msg-type 0)
                             (let ((mangled-data (split-string msg-data ",")))
                               (insert (text-color (format "%s %s " format-s (first mangled-data))))
                               (insert (sample-color (format "%s" (nth 1 mangled-data))))
                               (when (> (length mangled-data) 2)
                                 (insert (text-color (format " %s" (nthcdr 2 mangled-data)))))
                               (insert "\n")))
                           (when (and (not (= msg-type 4)) (not (= msg-type 0)))
                             (progn (insert (stdout-color (format "%s %s\n" format-s msg-data))))))
                         ))))
          )))

     ((string-match "/all-jobs-completed" level)
      (insert "π> ")
      (insert (info-color "(Live code is now dead code.)\n")))

     (t (insert (format "π> %s %s\n" level object))))))

(provide 'sonic-pi-console)

;;; sonic-pi-console.el ends here
