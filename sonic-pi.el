;;; sonic-pi.el --- A Emacs client for SonicPi

;; Copyright 2014 Joseph Wilk

;; Author: Joseph Wilk <joe@josephwilk.net>
;; URL: http://www.github.com/repl-electric/sonic-pi.el
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (osc "0.1") (dash "2.2.0") (emacs "24") (highlight "0"))
;; Keywords: SonicPi, Ruby

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Installation:

;;  M-x package-install sonic-pi
;;
;; ;;Set the location of your sonic-pi install
;; (setq sonic-pi-path \"YOUR_INSTALL_OF_SONIC_PI\")

;;; Usage:

;; M-x sonic-pi-jack-in

;;; Code:

(defgroup sonic-pi nil
  "A client for interacting with the SonicPi Server."
  :prefix "sonic-pi-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/repl-electric/sonic-pi.el")
  :link '(emacs-commentary-link :tag "Commentary" "Sonic Pi for Emacs"))

(require 'sonic-pi-mode)
(require 'sonic-pi-osc)

(defcustom sonic-pi-path
  nil
  "Path to install of sonicpi"
  :type 'string
  :group 'sonic-pi)

(defvar sonic-pi-server-bin             "server/bin/sonic-pi-server.rb")
(defvar sonic-pi-compile-extensions-bin "server/bin/compile-extensions.rb")
(defvar sonic-pi-margin-size 1)

(defun sonic-pi-server-cmd () (format "%s%s" sonic-pi-path sonic-pi-server-bin))

(defun sonic-pi--ruby-present-p ()
  "Check ruby is on the executable path"
  (executable-find "ruby"))

(defun sonic-pi--sonic-pi-server-present-p ()
  "Check sonic-pi server exists"
  (file-exists-p (format "%s/%s" sonic-pi-path sonic-pi-server-bin)))

(defun sonic-pi-valid-setup-p ()
  (cond
   ((not sonic-pi-path) (progn (message "No sonic-pi-path set! Did you forget (setq sonic-pi-path \"YOUR_INSTALL_OF_SONIC_PI\")")) nil)
   ((not (sonic-pi--sonic-pi-server-present-p)) (progn (message (format "Could not find a sonic-pi server in: %s/%s" sonic-pi-path sonic-pi-server-bin)) nil))
   ((not (sonic-pi--ruby-present-p)) (progn (message "Could not find a ruby (1.9.3+) executable to run SonicPi") nil))
   ((and sonic-pi-path (sonic-pi--sonic-pi-server-present-p) (sonic-pi--ruby-present-p)) t)
   (t nil)))

(defun sonic-pi-sonic-server-cleanup ()
  (when (get-process "sonic-pi-server")
    (delete-process "sonic-pi-server")))

;;;###autoload
(defun sonic-pi-jack-in (&optional prompt-project)
  "Boot and connect to the SonicPi Server"
  (interactive)
  (when (sonic-pi-valid-setup-p)
    (if (not (get-process "sonic-pi-server"))
        (let* ((cmd (sonic-pi-server-cmd)))
          (message "Starting SonicPi server...")
          (start-file-process-shell-command
           "sonic-pi-server"
           "*sonic-pi-server-messages*"
           cmd)))
    (set-window-margins (get-buffer-window) sonic-pi-margin-size)
    (sonic-pi-connect)
    (message "Ready!")))

;;;###autoload
(defun sonic-pi-connect (&optional prompt-project)
  "Assumes SonicPi server is running and connects"
  (interactive)
  (when (sonic-pi-valid-setup-p)
    (sonic-pi-osc-connect)
    (sonic-pi-messages-buffer-init)))

(provide 'sonic-pi)

;;; sonic-pi.el ends here
