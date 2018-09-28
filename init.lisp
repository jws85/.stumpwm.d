(in-package :stumpwm)

;; From https://github.com/alezost/stumpwm-config/blob/master/init.lisp
(defvar jws/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d")))
  "A directory with initially loaded files.")

(defvar jws/site-lisp-file
  (merge-pathnames "site.lisp" jws/init-directory)
  "A file containing site-specific configuration.")

(defvar jws/vendor-directory
  (merge-pathnames "vendor/" jws/init-directory)
  "A directory containing third-party modules.")

(defvar jws/data-directory
  (merge-pathnames "data/" jws/init-directory)
  "A directory containing dumped window/frame data.")

(defvar jws/group-data-file
  (namestring (merge-pathnames "groups.lisp" jws/data-directory))
  "A file containing dumped group data.")

(defun jws/load (filename)
  "Load a file FILENAME (without extension) from `al/init-directory'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               jws/init-directory)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(set-module-dir jws/vendor-directory)

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

;; Colors --------------------------------------------------------------------

(jws/load "colors")

(setf *color-scheme* *gruvbox-dark-colors*)

(set-color-scheme *color-scheme*)
(set-fg-color (get-white *color-scheme*))
(set-bg-color (get-black *color-scheme*))
(set-border-color (get-black *color-scheme*))
(set-focus-color (slot-value *color-scheme* 'magenta))
(set-float-focus-color (slot-value *color-scheme* 'magenta))
(set-float-unfocus-color (slot-value *color-scheme* 'blue))

;; Groups --------------------------------------------------------------------

;; I've run into some issues with setting groups.  Stump insists on starting
;; with one group called "Default".  When you restore from a file, it dumps
;; all of those groups on top of that.
;;
;; So what I do now is:
;;  - Get a variable associated with the Default group
;;  - Rename it
;;  - Restore my other groups (I hand-hack the dump to remove Default)
;;  - Switch back to the Default group
(let ((main-group (first (screen-groups (current-screen)))))
  (setf (group-name main-group) "main")
  (if (probe-file jws/group-data-file)
      (restore-from-file jws/group-data-file))
  (gselect main-group))

;; Swank server --------------------------------------------------------------

;; https://kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/index.html
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

;; Messages ------------------------------------------------------------------

(setq *message-window-gravity* :top-left)

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
    (define-key map (kbd "k") "describe-key")
    (define-key map (kbd "q") "quit")
    (define-key map (kbd "d") (concatenate
                               'string "dump-desktop-to-file "
                               jws/group-data-file))
    (define-key map (kbd "i") "info")
    (define-key map (kbd "I") "show-window-properties")
    (define-key map (kbd "SPC") "eval")
    (define-key map (kbd ":") "colon")
    map))
(define-key *top-map* (kbd "s-q") *stumpwm-control-map*)

;; Group handling
(define-key *top-map* (kbd "s-g") "gnewbg")
(define-key *top-map* (kbd "s-G") "gnewbg-float")
(define-key *top-map* (kbd "s-d") "gkill")
(define-key *top-map* (kbd "s-r") "grename")
(define-key *top-map* (kbd "s-Left") "gprev")
(define-key *top-map* (kbd "s-Right") "gnext")
(define-key *top-map* (kbd "s-TAB") "gother")
(define-key *top-map* (kbd "s-`") "grouplist")
(define-key *top-map* (kbd "s-~") "gmove")
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
(define-key *top-map* (kbd "s-H") "exchange-direction left")
(define-key *top-map* (kbd "s-J") "exchange-direction down")
(define-key *top-map* (kbd "s-K") "exchange-direction up")
(define-key *top-map* (kbd "s-L") "exchange-direction right")

;; Window and frame operations
(define-key *top-map* (kbd "s--") "vsplit")
(define-key *top-map* (kbd "s-\\") "hsplit")
(define-key *top-map* (kbd "s-,") "resize -20 0")
(define-key *top-map* (kbd "s-.") "resize +20 0")
(define-key *top-map* (kbd "s-<") "resize 0 -20")
(define-key *top-map* (kbd "s->") "resize 0 +20")
(define-key *top-map* (kbd "s-|") "hsplit")
(define-key *top-map* (kbd "s-x") "remove-split")
(define-key *top-map* (kbd "s-X") "only")
(define-key *top-map* (kbd "s-=") "balance-frames")
(define-key *top-map* (kbd "s-/") "windowlist")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "s-p") "prev-in-frame")
(define-key *top-map* (kbd "s-o") "other-window")
(define-key *top-map* (kbd "M-TAB") "other-window")
(define-key *top-map* (kbd "s-c") "delete")
(define-key *top-map* (kbd "s-C") "kill")

(define-key *top-map* (kbd "s-z") "banish")

;; Applications
(jws/load "audio")

(define-key *top-map* (kbd "s-SPC") "exec rofi -show run -display-run \">>> \"")
(define-key *top-map* (kbd "s-RET") "exec sakura")
(define-key *top-map* (kbd "s-e") "exec emacsclient -c")
(define-key *top-map* (kbd "s-b") "exec feh --bg-scale ~/.wallpaper")
(define-key *top-map* (kbd "s-ESC") "exec i3lock -c282828")
(define-key *top-map* (kbd "SunPrint_Screen") "exec scrot")

;; Modeline ------------------------------------------------------------------

(enable-mode-line (current-screen) (current-head) t)

(setf *time-modeline-string* "%Y-%m-%e %H:%M")
(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* (get-black *gruvbox-dark-colors*))
(setf *mode-line-foreground-color* (get-white *gruvbox-dark-colors*))

(load-module "stumptray")
(defun jws/build-modeline (left right)
  (setf *screen-mode-line-format* (concatenate 'string left "^>" right " %T")))
(jws/build-modeline "^B^5 %g ^*^b• ^B^4%W^*^b" "^B%d^b")

;; Modeline ------------------------------------------------------------------

(if (probe-file jws/site-lisp-file)
    (load jws/site-lisp-file))

;; Stumptray -----------------------------------------------------------------

;; Must be loaded last...
;; (ql:quickload "xembed")
(stumptray:stumptray)
