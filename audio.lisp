(in-package :stumpwm)

;; (ql:quickload "split-sequence")
;; (ql:quickload "cl-ppcre")

(defun jws/split-string-by-newlines (string)
  (split-sequence:split-sequence #\linefeed string :remove-empty-subseqs t))

(defun jws/get-master-mixer-status ()
  (let* ((output (run-shell-command "amixer sget Master" t))
         (lines (jws/split-string-by-newlines output))
         (regex "\\[([^\\]]+)\\] \\[[^\\]]+\\] \\[([^\\]]+)\\]"))
    (cl-ppcre:register-groups-bind (volume mute) (regex (car (last lines)))
      (format nil "Volume: ~A" (if (string= mute "off") "Muted" volume)))))

(defcommand volume-up () ()
  (run-shell-command "pactl set-sink-volume 0 +5%")
  (message (jws/get-master-mixer-status)))
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")

(defcommand volume-down () ()
  (run-shell-command "pactl set-sink-volume 0 -5%")
  (message (jws/get-master-mixer-status)))
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")

(defcommand volume-toggle-mute () ()
  (run-shell-command "pactl set-sink-mute 0 toggle")
  (message (jws/get-master-mixer-status)))
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")
