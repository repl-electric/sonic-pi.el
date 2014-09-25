;;; sonic-osc.el --- Object inspector -*- lexical-binding: t -*-

(require 'osc)
(require 'sonic-console)

(defvar sonic-osc-client nil "Connection to send msgs to sonic pi")
(defvar sonic-osc-server nil "Connection to recieve msgs from sonic pi")

(defun sonic-osc-connect ()
  (setq sonic-osc-client (sonic-osc-make-client "localhost" 4557))
  (if (not sonic-osc-server)
      (setq sonic-osc-server
            (sonic-osc-make-server "localhost" 4558
                                   (lambda (_ &rest args) (sonic-log-message args))))))

(defun sonic-osc-send-text (start end)
  (osc-send-message sonic-osc-client "/run-code" (buffer-substring-no-properties start end)))

(defun sonic-send-region ()
  "send a region to sonic via osc"
  (interactive)
  (sonic-osc-send-text (region-beginning) (region-end)))

(defun sonic-send-buffer ()
  "send the current buffer to sonic via osc"
  (interactive)
  (sonic-osc-send-text (point-min) (point-max)))

(defun sonic-osc-make-client (host port)
  (make-network-process
   :name "OSC client"
   :host host
   :service port
   :type 'datagram
   :family 'ipv4))

(defun sonic-osc-make-server (host port default-handler)
  (make-network-process
   :name "OSC server"
   :host host
   :server t
   :service port
   :filter #'osc-filter
   :type 'datagram
   :family 'ipv4
   :plist (list :generic default-handler)))

(defun sonic-cleanup ()
  "kill things"
  (delete-process sonic-osc-client)
  (delete-process sonic-osc-server)
  (setq sonic-osc-server nil)
  (setq sonic-osc-client nil))

(defun sonic-ping ()
  (interactive)
  (osc-send-message sonic-osc-client "/ping" "hi"))

(provide 'sonic-osc)

;;; sonic-osc.el ends here
