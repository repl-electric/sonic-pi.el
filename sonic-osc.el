(require 'osc)

;;(setf sonic-server (osc-make-server "localhost" 4558))

(defun sonic-osc-connect ()
  (interactive)
  (setf sonic-client (osc-make-client "localhost" 4557)))

(defun sonic-send-text (start end)
  (osc-send-message sonic-client "/run-code" (buffer-substring-no-properties start end)))

(defun sonic-send-region ()
  "send a region to sonic via osc"
  (interactive)
  (sonic-send-text (region-beginning) (region-end)))

(defun sonic-send-buffer ()
  "send the current buffer to sonic via osc"
  (interactive)
  (sonic-send-text (point-min) (point-max)))

(defun osc-make-client (host port)
  (make-network-process
   :name "OSC client"
   :host host
   :service port
   :type 'datagram
   :family 'ipv4))

(defun sonic-cleanup ()
  "kill things"
  (interactive)
  (delete-process sonic-client))

(defun sonic-ping ()
  (interactive)
  (osc-send-message sonic-client "/ping" "hi"))

(provide 'sonic-osc)
