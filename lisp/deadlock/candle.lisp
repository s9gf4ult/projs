(in-package :deadlock)

(defclass candle()
  ((open :initarg :open :reader candle-open)
   (close :initarg :close :reader candle-close)
   (high :initarg :high :reader candle-high)
   (low :initarg :low :reader candle-low)
   (volume :initarg :volume :reader candle-volume)
   (datetime :initarg :datetime :reader candle-datetime)
   (type :reader candle-type)
   (period :initarg :type :initform :s1 :reader candle-period :documentation "can be :s1 :m1 :m5 :h1 :d1 :month :year")))

(defmethod shared-initialize :after ((obj candle) slot-names &rest initargs &key)
  (setf (slot-value obj 'type) (cond
                                 ((> (candle-close obj) (candle-open obj)) :long)
                                 ((< (candle-close obj) (candle-open obj)) :short)
                                 (t :empty))))