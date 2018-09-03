(in-package :stumpwm)

(set-module-dir "~/.stumpwm.d/vendor/")

;; Modules -------------------------------------------------------------------

;; (ql:quickload "dbus") ;; which installs ALL THE THINGS
;; (ql:quickload "xml-emitter")
(load-module "notify")
(notify:notify-server-toggle)

;; Fonts ---------------------------------------------------------------------

;; (ql:quickload "clx-truetype")
;; (nconc xft:*font-dirs '("/home/yourname/.local/share/fonts"))
;; (xft:cache-fonts)
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "PragmataPro Mono" :subfamily "Regular" :size 10.5))

;; Swank server --------------------------------------------------------------

;; https://kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/index.html
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

;; Switching groups ----------------------------------------------------------

(setq *focus-group-hook* nil)
(add-hook *focus-group-hook*
          (lambda (new old)
            (echo (format nil "Switched to^5 ~D^*:^6~A^*!"
                          (group-number new) (group-name new)))))

;; Key bindings --------------------------------------------------------------

(set-prefix-key (kbd "s-q"))

;; StumpWM operations
(defvar *stumpwm-control-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") "reload")
    (define-key map (kbd "s") "mode-line")
    (define-key map (kbd "R") "loadrc")
    (define-key map (kbd "Q") "quit")
    (define-key map (kbd "i") "info")
    (define-key map (kbd "I") "show-window-properties")
    (define-key map (kbd "SPC") "eval")
    map))
(define-key *top-map* (kbd "s-ESC") *stumpwm-control-map*)

;; Group handling
(define-key *top-map* (kbd "s-g") "gnew")
(define-key *top-map* (kbd "s-r") "grename")
(define-key *top-map* (kbd "s-TAB") "gother")
(define-key *top-map* (kbd "s-`") "grouplist")
(defun normalize-numbers (num)
  (if (= num 0) 10 num))
(defun get-shifted-number (num)
  (char ")!@#$%^&*(" num))
(dolist (num (alexandria:iota 10))
  (define-key *top-map* (kbd (format nil "s-~D" num))
    (format nil "gselect ~D" (normalize-numbers num)))
  (define-key *top-map* (kbd (format nil "s-~C" (get-shifted-number num)))
    (format nil "gmove ~D" (normalize-numbers num))))

;; Move frame focus
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")

;; Move current window
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

;; Window and frame operations
(define-key *top-map* (kbd "s--") "vsplit")
(define-key *top-map* (kbd "s-\\") "hsplit")
(define-key *top-map* (kbd "s-|") "hsplit")
(define-key *top-map* (kbd "s-x") "remove-split")
(define-key *top-map* (kbd "s-X") "only")
(define-key *top-map* (kbd "s-=") "balance-frames")
(define-key *top-map* (kbd "s-w") "windowlist")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")
(define-key *top-map* (kbd "s-c") "delete")
(define-key *top-map* (kbd "s-C") "kill")

;; Applications
(define-key *top-map* (kbd "s-SPC") "exec rofi -show run -display-run \">>> \"")
(define-key *top-map* (kbd "s-RET") "exec sakura")

;; Modeline ------------------------------------------------------------------

;; (defun jws/timestamp ()
;;    (multiple-value-bind
;;          (sec min hr day mon yr dow dstp tz)
;;        (get-decoded-time)
;;      (format t "~4,'0d-~2,'0d-~2,'0d ~2,'Od:~2,'0d:~2,'0d"
;;              yr mon day hr min sec)))

;; (enable-mode-line (current-screen) (current-head) t)
;; (setf *screen-mode-line-format* '("%w | " (jws/timestamp)))

;; Startup applications ------------------------------------------------------

(run-shell-command "/usr/bin/setxkbmap -option ctrl:swapcaps")
