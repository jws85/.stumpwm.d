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
(set-font (make-instance 'xft:font :family "PragmataPro Mono" :subfamily "Regular" :size 11))

;; Swank server --------------------------------------------------------------

;; https://kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/index.html
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

;; Messages ------------------------------------------------------------------

(setq *focus-group-hook* nil)
(add-hook *focus-group-hook*
          (lambda (new old)
            (echo (format nil "Switched to^B^4 ~D^*:^B^5~A^*^b!"
                          (group-number new) (group-name new)))))

(setf *startup-message* "*Welcome to ^B^5StumpWM^*^b! ^_^")

;; Key bindings --------------------------------------------------------------

;; Raaaaaaaaaggggghhhhhhhhhhhhhhhhhhh
(set-prefix-key (kbd "C-M-H-s-z"))

;; StumpWM operations
(defvar *stumpwm-control-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") "reload")
    (define-key map (kbd "s") "mode-line")
    (define-key map (kbd "R") "loadrc")
    (define-key map (kbd "q") "quit")
    (define-key map (kbd "i") "info")
    (define-key map (kbd "I") "show-window-properties")
    (define-key map (kbd "SPC") "eval")
    map))
(define-key *top-map* (kbd "s-q") *stumpwm-control-map*)

;; Group handling
(define-key *top-map* (kbd "s-g") "gnew")
(define-key *top-map* (kbd "s-d") "gkill")
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

(define-key *top-map* (kbd "s-z") "banish")

;; Applications
(define-key *top-map* (kbd "s-SPC") "exec rofi -show run -display-run \">>> \"")
(define-key *top-map* (kbd "s-RET") "exec sakura")
(define-key *top-map* (kbd "s-b") "exec feh --bg-scale ~/.wallpaper")
(define-key *top-map* (kbd "s-ESC") "exec i3lock -c282828")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pactl set-sink-volume 0 +5%")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pactl set-sink-volume 0 -5%")
(define-key *top-map* (kbd "XF86AudioMute") "exec pactl set-sink-mute 0 toggle")

;; Modeline ------------------------------------------------------------------

(enable-mode-line (current-screen) (current-head) t)

(defclass color-scheme ()
  ((black :initarg :black :initform "black")
   (red :initarg :red :initform "red")
   (green :initarg :green :initform "green")
   (yellow :initarg :yellow :initform "yellow")
   (blue :initarg :blue :initform "blue")
   (magenta :initarg :magenta :initform "magenta")
   (cyan :initarg :cyan :initform "cyan")
   (white :initarg :white :initform "white")))

(defmethod get-color-list ((scheme color-scheme))
  (list (slot-value scheme 'black)
        (slot-value scheme 'red)
        (slot-value scheme 'green)
        (slot-value scheme 'yellow)
        (slot-value scheme 'blue)
        (slot-value scheme 'magenta)
        (slot-value scheme 'cyan)
        (slot-value scheme 'white)))

(defmethod set-color-scheme ((scheme color-scheme))
  (setf *colors* (get-color-list scheme))
  (update-color-map (current-screen)))

(defmethod get-black ((scheme color-scheme))
  (slot-value scheme 'black))

(defmethod get-white ((scheme color-scheme))
  (slot-value scheme 'white))

(setf *gruvbox-dark-colors*
      (make-instance 'color-scheme
                     :black "#282828"
                     :red "#cc241d"
                     :green "#98971a"
                     :yellow "#d79921"
                     :blue "#458588"
                     :magenta "#b16286"
                     :cyan "#689d6a"
                     :white "#a89984"))
(set-color-scheme *gruvbox-dark-colors*)

(set-fg-color (get-white *gruvbox-dark-colors*))
(set-bg-color (get-black *gruvbox-dark-colors*))
(set-border-color (get-black *gruvbox-dark-colors*))
(set-focus-color (slot-value *gruvbox-dark-colors* 'magenta))

(setf *time-modeline-string* "%Y-%m-%e %H:%M")
(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* (get-black *gruvbox-dark-colors*))
(setf *mode-line-foreground-color* (get-white *gruvbox-dark-colors*))
(setf *screen-mode-line-format* "^B%d^b •^B^5 %g ^*^b• ^B^4%W^*^b")

;; (ql:quickload "xembed")
(load-module "stumptray")
(stumptray:stumptray)
