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
  (with-slots (black red green yellow blue magenta cyan white)
      scheme
    (list black red green yellow blue magenta cyan white)))

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
