(in-package :stumpwm)

(set-module-dir "~/.stumpwm.d/vendor/")

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

;; Key bindings --------------------------------------------------------------

(set-prefix-key (kbd "s-q"))

;; StumpWM operations
(define-key *root-map* (kbd "R") "loadrc")
(define-key *root-map* (kbd "C-r") "reload")
(define-key *root-map* (kbd "Q") "quit")

;; Group handling
(define-key *top-map* (kbd "s-g") "gnew")
(define-key *top-map* (kbd "s-TAB") "gother")
(defun normalize-numbers (num)
  (if (= num 0) 10 num))
(dolist (num (alexandria:iota 10))
  (define-key *top-map* (kbd (format nil "s-~D" num))
    (format nil "gselect ~D" (normalize-numbers num))))

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

;; Frame operations
(define-key *top-map* (kbd "s--") "vsplit")
(define-key *top-map* (kbd "s-\\") "hsplit")
(define-key *top-map* (kbd "s-|") "hsplit")
(define-key *top-map* (kbd "s-c") "remove-split")
(define-key *top-map* (kbd "s-C") "only")
(define-key *top-map* (kbd "s-=") "balance-frames")

;; Applications
(define-key *top-map* (kbd "s-SPC") "exec rofi -show run -display-run \">>> \"")
;(define-key *root-map* (kbd "F1") "exec firefox")
;(define-key *root-map* (kbd "F2") "exec emacs")
;(define-key *root-map* (kbd "F3") "exec sakura")

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

;; (ql:quickload "dbus") ;; which installs ALL THE THINGS
;; (ql:quickload "xml-emitter")
(load-module "notify")
(notify:notify-server-toggle)

(run-shell-command "/usr/bin/setxkbmap -option ctrl:swapcaps")
