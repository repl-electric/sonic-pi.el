;;; sonic-pi-osc.el --- Manage OSC client and server for SonicPi communication -*- lexical-binding: t -*-

(require 'osc)
(require 'cl-lib)
(require 'highlight)

(require 'sonic-pi-console)

(defvar flash-time 0.5)

(defface eval-sonic-pi-flash
  '((((class color)) (:background "#F23444" :foreground "white" :bold nil))
    (t (:inverse-video t)))
  "Face for highlighting sexps during evaluation."
  :group 'eval-sonic-pi)
(defface eval-sonic-pi-flash-error
  '((((class color)) (:foreground "red" :bold nil))
    (t (:inverse-video t)))
  "Face for highlighting sexps signaled errors during evaluation."
  :group 'eval-sonic-pi)


(defvar sonic-pi-osc-client nil "Connection to send msgs to sonic pi")
(defvar sonic-pi-osc-server nil "Connection to recieve msgs from sonic pi")

(defun sonic-pi-osc-message-handler (path &rest args)
  (interactive)
  (sonic-pi-log-message path args))

(defun sonic-pi-osc-connect ()
  (if sonic-pi-osc-client   (delete-process sonic-pi-osc-client))
  (setq sonic-pi-osc-client (sonic-pi-osc-make-client "localhost" 4557))
  (if (not sonic-pi-osc-server)
      (setq sonic-pi-osc-server
            (sonic-pi-osc-make-server "localhost" 4558
                                      'sonic-pi-osc-message-handler))))

(defun sonic-pi-osc-send-command-with-arg (cmd arg1 arg2)
  (if sonic-pi-osc-client
      (osc-send-message sonic-pi-osc-client (format "/%s" cmd) arg1 arg2)
    (message "Sonic-pi not running... `sonic-pi-jack-in` or `sonic-pi-connect`")))

(defun sonic-pi-osc-send-command-with-arg4 (cmd arg1 arg2 arg3 arg4)
  (if sonic-pi-osc-client
      (osc-send-message sonic-pi-osc-client (format "/%s" cmd) arg1 arg2 arg3 arg4)
    (message "Sonic-pi not running... `sonic-pi-jack-in` or `sonic-pi-connect`")))


(defun sonic-pi-osc-send-command (cmd)
  (if sonic-pi-osc-client
      (osc-send-message sonic-pi-osc-client (format "/%s" cmd))
    (message "Sonic-pi not running... `sonic-pi-jack-in` or `sonic-pi-connect`")))

(defun sonic-pi-osc-send-text (start end)
  (sonic-pi-osc-send-command-with-arg4 "save-and-run-buffer"
                                      "sonicpi-emacs"
                                      (buffer-name)
                                      (buffer-substring-no-properties start end)
                                      (buffer-name)))

(defun sonic-pi-osc-send-file ()
  (sonic-pi-osc-send-command-with-arg4 "save-and-run-buffer-via-local-file"
                                       "sonicpi-emacs"
                                       (buffer-name)
                                       (buffer-file-name)
                                       (buffer-name)))

(defun sonic-pi-send-region ()
  "send a region to sonic via osc"
  (interactive)
  (sonic-pi-osc-send-text (region-beginning) (region-end))
  (hlt-highlight-regexp-region (region-beginning) (region-end) ".+" 'eval-sonic-pi-flash nil)
  (run-at-time flash-time nil 'hlt-unhighlight-region nil nil nil))

(defun sonic-pi-send-line ()
  "send a line to sonic via osc"
  (interactive)
  (sonic-pi-osc-send-text (line-beginning-position) (line-end-position))
  (hlt-highlight-regexp-region (line-beginning-position) (line-end-position) ".+" 'eval-sonic-pi-flash nil)
  (run-at-time flash-time nil 'hlt-unhighlight-region nil nil nil))

(defun sonic-pi-send-buffer ()
  "send the current buffer to sonic via osc"
  (interactive)


  ;;TODO: I don't understand overlays very well. Something other overlay is blocking our overlay
  ;;When we remove just ours, we never see any new overlays appear :(
  ;;(remove-overlays (window-start) (window-end) 'sonic-pi-gutter t)
  (dolist (o (overlays-in (window-start) (window-end)))
    (delete-overlay o)
    ;;(when (overlay-get o 'sonic-pi-gutter) (delete-overlay o))
    )
  (sonic-pi-osc-send-text (point-min) (point-max))
                          ;; NOTE: to fix issue [https://github.com/repl-electric/sonic-pi.el/issues/21] use
                          ;; (save-buffer)
                          ;;(sonic-pi-osc-send-file)
  (hlt-highlight-regexp-region nil nil ".+" 'eval-sonic-pi-flash nil)
  (run-at-time flash-time nil 'hlt-unhighlight-region))

(defun sonic-pi-send-live-loop ()
  "send a live-loop to sonic via osc"
  (interactive)
  (save-excursion
    (let ((s (re-search-backward "^\\(live_loop\\|with_fx\\|in_thread\\|define\\)")))
      (ruby-end-of-block)
      (end-of-line)
      (sonic-pi-osc-send-text s (point))
      (hlt-highlight-regexp-region s (point) ".+" 'eval-sonic-pi-flash nil))
    (run-at-time flash-time nil 'hlt-unhighlight-region nil nil nil)))

(defun sonic-pi-osc-make-client (host port)
  (make-network-process
   :name "sonic-pi.el OSC client"
   :host host
   :service port
   :type 'datagram
   :family 'ipv4))

(defun sonic-pi-osc-make-server (host port default-handler)
  (make-network-process
   :name "sonic-pi.el OSC server"
   :host host
   :server t
   :service port
   :filter #'osc-filter
   :type 'datagram
   :family 'ipv4
   :plist (list :generic default-handler)))

(defun sonic-pi-osc-cleanup ()
  "Remove osc server and client"
  (when sonic-pi-osc-client
    (delete-process sonic-pi-osc-client))
  (when sonic-pi-osc-server
    (delete-process sonic-pi-osc-server))
  (setq sonic-pi-osc-server nil)
  (setq sonic-pi-osc-client nil))

(defun sonic-pi-ping ()
  "Test if sonic pi server is really, really there."
  (interactive)
  (osc-send-message sonic-pi-osc-client "/ping" "hi"))

(provide 'sonic-pi-osc)

;;; sonic-pi-osc.el ends here
