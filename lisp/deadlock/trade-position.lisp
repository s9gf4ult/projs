(in-package :deadlock)

(defclass trade-position ()
  ((open-time :initform nil :initarg :time :reader open-time)
   (close-time :initform nil :accessor close-time)
   (open-coast :initform nil :initarg :open-coast :reader open-coast)
   (close-coast :initform nil :accessor close-coast)
   (position-type :initform :closed :initarg :position-type :reader position-type :documentation "can be :short or :long :closed")
   (back-stop :initform nil :initarg :back-stop :accessor back-stop)
   (profit-stop :initform nil :initarg :profit-stop :accessor profit-stop)))

(defgeneric opened? (position))
(defgeneric close-position (pos close-time close-coast))

(defmethod opened? ((position trade-position))
  (and (open-time position)
       (open-coast position)
       (not (eq :closed (position-type position)))))

(defmethod close-position ((pos trade-position) close-time close-coast)
  (setf (close-time pos) close-time
        (close-coast pos) close-coast
        (slot-value pos 'position-type) :closed))