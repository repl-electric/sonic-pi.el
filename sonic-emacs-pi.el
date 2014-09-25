(require 'sonic-mode)
(require 'sonic-osc)

(defgroup sonic-emacs-pi nil
  "A client for interacting with the SonicPi Server."
  :prefix "sonic-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/repl-electric/sonic-emacs-pi")
  :link '(emacs-commentary-link :tag "Commentary" "Sonic Emacs Pi"))

(defvar sonic-pi-path "/Users/josephwilk/Workspace/josephwilk/ruby/sonic-pi")
(defvar sonic-pi-server-bin "app/server/bin/sonic-pi-server.rb")

;;;###autoload
(defun sonic-jack-in (&optional prompt-project)
  "Start SonicPi Server"
  (interactive)
  (let* ((cmd (format "%s/%s" sonic-pi-path sonic-pi-server-bin)))
    (start-file-process-shell-command
     "sonic-server"
     "sonic-sonic-boom"
     cmd)))

;;;###autoload
(defun sonic-connect (&optional prompt-project)
  "Connect to SonicPi Server"
  (interactive)
  (sonic-osc-connect))

;;;###autoload
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c M-j") 'sonic-jack-in)
     (define-key ruby-mode-map (kbd "C-c M-c") 'sonic-connect)
     (define-key ruby-mode-map (kbd "C-c C-k") 'sonic-send-buffer)

     ))

(provide 'sonic-emacs-pi)
