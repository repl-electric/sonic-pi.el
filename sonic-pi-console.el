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
(defconst sonic-pi-mid-str   "   ├─")
(defconst sonic-pi-end-str   "   └─")
(defconst sonic-pi-start-str "   ├─")

(defconst sonic-pi-ignore-cues 1)

(defconst sonic-pi-message-buffer-intro
  "Welcome to Sonic Pi d[-_-]b
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
  "Return or create the buffer given by `sonic-pi-message-buffer-name' .
The default buffer name is *sonic-pi-messages*                         . "
  (or (get-buffer sonic-pi-message-buffer-name)
      (let ((buffer (get-buffer-create sonic-pi-message-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (setq-local comment-start ";")
          (setq-local comment-end ""))
        buffer)))


(defface sonic-pi-error-marker
   '((t (:foreground "yellow" :weight bold :inherit default)))
   "Face of error marker")

(defun sonic-pi-log-message (level msg)
  "Log the given MSG to the buffer given by `sonic-pi-message-buffer-name'."
  (interactive)
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
            (error-marker (str) (propertize str 'face `(:weight ultra-bold :foreground , "red")))
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
      (progn
        (let ((error-msg
               (replace-regexp-in-string
                "&#39" "'"
                (format "%s" (cl-second object)))))
          (message (format "Error: %s" error-msg))
          (insert (error-color
                   (format "π> Syntax Error: %s\n" error-msg)))

        (save-match-data ; is usually a good idea
          (and (string-match "line\s+\\([0-9]+\\)" (cl-second object))
               (setq line-error (string-to-number (format "%s" (match-string 1 (cl-second object)))))))

        (save-match-data ; is usually a good idea
          (and (string-match "buffer\s+\\(.+\\)," (cl-second object))
               (setq error-buffer (format "%s" (match-string 1 (cl-second object))))))

        (when (not (string= error-buffer "eval"))
          (with-current-buffer (get-file-buffer error-buffer)
            (save-excursion
              (let ((error-line line-error))
                (goto-line error-line)
                (let ((ov (make-overlay (line-beginning-position) (+ 1 (line-beginning-position)))))
                  (overlay-put ov 'priority 2)
                  (overlay-put ov
                               'before-string
                               (propertize " "
                                           'display
                                           `((margin left-margin)
                                             , (error-marker "\u25B6"))))
                  (overlay-put ov 'sonic-pi-gutter t)
                  (overlay-put ov 'evaporate t))))))

          )))
     ((string-match "\/error" level)
      (progn
        (save-match-data ; is usually a good idea
          (and (string-match "line\s+\\([0-9]+\\)" (cl-second object))
               (setq line-error (string-to-number (format "%s" (match-string 1 (cl-second object)))))))

        (save-match-data ; is usually a good idea
          (and (string-match "buffer\s+\\(.+\\)," (cl-second object))
               (setq error-buffer (format "%s" (match-string 1 (cl-second object))))))

        (when (not (string= error-buffer "eval"))
                  (with-current-buffer (get-file-buffer error-buffer)
                    (save-excursion
                      (let ((error-line line-error))
                        (goto-line error-line)
                        (let ((ov (make-overlay (line-beginning-position) (+ 1 (line-beginning-position)))))

                          (overlay-put ov 'priority 2)
                          (overlay-put ov
                                       'before-string
                                       (propertize " "
                                                   'display
                                                   `((margin left-margin)
                                                     , (error-marker "\u25B6"))))
                          (overlay-put ov 'sonic-pi-gutter t)
                          (overlay-put ov 'evaporate t))))))

        (insert (error-color (replace-regexp-in-string
                              "&#39" "'"
                              (replace-regexp-in-string "&gt;" ">"
                                                        (format "π> Error: %s\n" (cl-second object))))))))

     ((string-match "\/multi_message*" level)
      ;;TODO: multi_message does not batch messages together,
      ;;so we get them individual without msg-count being incremented.
      ;;Means we duplicate the Run information per message
      (progn
        (let ((job-id (cl-first object))
              (thread-name (cl-second object))
              (run-time (cl-third object))
              (msg-count (cl-fourth object))
              (data (nthcdr 4 object)))
          (when (or (> msg-count 1) (string= "" thread-name) )
            (progn
              (insert "[")
              (if (not (string= "" thread-name))
                  (insert (format "%s" (thread-color thread-name)))
                (insert (format "%s" (thread-color "master")))
                )
              (insert "]\n")
              (cl-loop for msg-type in data by (-partial 'nthcdr 2)
                       for msg-data in (cdr data) by (-partial 'nthcdr 2)
                       for idx from 0 to msg-count
                       do
                       (let ((format-s
                              (if (or
                                   (and (string= "" thread-name) (= idx (- msg-count 1)))
                                   (= idx (- msg-count 1)))
                                  sonic-pi-end-str
                                sonic-pi-mid-str)))
                         (progn
                           (when (and (= msg-type 4) (= sonic-pi-ignore-cues 0))
                             (progn (insert (sample-color (format "%s%s\n" format-s msg-data)))))
                           (when (= msg-type 0)
                             (let ((mangled-data (split-string msg-data ",")))
                               (insert (text-color (format "%s%s " format-s (cl-first mangled-data))))
                               (insert (sample-color (format "%s" (nth 1 mangled-data))))
                               (when (> (length mangled-data) 2)
                                 (insert (text-color (format " %s" (nthcdr 2 mangled-data)))))
                               (insert "\n")))
                           (when (and (not (= msg-type 4)) (not (= msg-type 0)))
                             (progn (insert (stdout-color (format "%s%s\n" format-s msg-data))))))
                         ))))
          )))

     ((string-match "/all-jobs-completed" level)
      (insert "π> ")
      (insert (info-color "(Live code is now dead code.)\n")))

     (t (insert (format "π> %s %s\n" level object))))))

(provide 'sonic-pi-console)

;;;
