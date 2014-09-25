;;; sonic-emacs-pi.el --- A Emacs client for SonicPi
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (osc "0.1") (emacs "24"))
;; Keywords: SonicPi, Ruby

(require 'sonic-mode)
(require 'sonic-osc)

(defgroup sonic-emacs-pi nil
  "A client for interacting with the SonicPi Server."
  :prefix "sonic-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/repl-electric/sonic-emacs-pi")
  :link '(emacs-commentary-link :tag "Commentary" "Sonic Emacs Pi"))

(defcustom sonic-pi-path "/Users/josephwilk/Workspace/josephwilk/ruby/sonic-pi" "Path to install of sonicpi")
(defvar    sonic-pi-server-bin "app/server/bin/sonic-pi-server.rb")

;;;###autoload
(defun sonic-jack-in (&optional prompt-project)
  "Boot and connect to the SonicPi Server"
  (interactive)
  (let* ((cmd (format "rvm use 1.9.3-p545; %s/%s" sonic-pi-path sonic-pi-server-bin)))
    (start-file-process-shell-command
     "sonic-server"
     "sonic-sonic-boom"
     cmd))
  (sonic-connect))

;;;###autoload
(defun sonic-connect (&optional prompt-project)
  "Connect to SonicPi Server"
  (interactive)
  (sonic-osc-connect))

;;;###autoload
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c M-j") 'sonic-jack-in)
     (define-key ruby-mode-map (kbd "C-c M-c") 'sonic-connect)))

(provide 'sonic-emacs-pi)

;;; sonic-emacs-pi.el ends here
