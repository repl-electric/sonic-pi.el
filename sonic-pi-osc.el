;;; sonic-pi-osc.el --- Manage OSC client and server for SonicPi communication -*- lexical-binding: t -*-

(require 'osc)
(require 'sonic-pi-console)

(defvar sonic-pi-osc-client nil "Connection to send msgs to sonic pi")
(defvar sonic-pi-osc-server nil "Connection to recieve msgs from sonic pi")

(defun sonic-pi-osc-connect ()
  (if sonic-pi-osc-client   (delete-process sonic-pi-osc-client))
  (setq sonic-pi-osc-client (sonic-pi-osc-make-client "localhost" 4557))
  (if (not sonic-pi-osc-server)
      (setq sonic-pi-osc-server
            (sonic-pi-osc-make-server "localhost" 4558
                                   (lambda (path &rest args) (sonic-pi-log-message path args))))))

(defun sonic-pi-osc-send-text (start end)
  (if sonic-pi-osc-client
      (osc-send-message sonic-pi-osc-client "/run-code" (buffer-substring-no-properties start end))
    (message "Sonic-pi not running... `sonic-pi-jack-in` or `sonic-pi-connect`")))

(defun sonic-pi-send-region ()
  "send a region to sonic via osc"
  (interactive)
  (sonic-pi-osc-send-text (region-beginning) (region-end)))

(defun sonic-pi-send-buffer ()
  "send the current buffer to sonic via osc"
  (interactive)
  (sonic-pi-osc-send-text (point-min) (point-max)))

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
