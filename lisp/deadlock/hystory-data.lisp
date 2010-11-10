(in-package :deadlock)

(defmacro with-candles (hystory candle period &body body)
  (let ((hyst (gensym))
        (open (gensym))
        (close (gensym))
        (datetime (gensym))
        (high (gensym))
        (low (gensym))
        (volume (gensym)))
    `(let ((,hyst (sqlite-handle ,hystory)))
       (with-sqlite-select ,hyst ((,datetime "datetime") (,open "open") (,close "close") (,high "high") (,low "low") (,volume "volume")) ,(if period
                                                                            "select * from candles where datetime between ? and ?"
                                                                            "select * from candles") ,period
         (let ((,candle (make-instance 'candle :datetime ,datetime :open ,open :close ,close :high ,high :low ,low :volume ,volume)))
               ,@body)))))
             
     

(defclass hystory-data ()
  ((sqlite-handle :initform nil :initarg :sqlite-handle :accessor sqlite-handle)
   (file-name :initform nil :initarg :file-name :reader file-name)))

(defgeneric finalize-hystory (hyst))
(defgeneric

(defmethod shared-initialize :after ((obj hystory-data) slot-names &rest initarts &key)
  (if (not (sqlite-handle obj))
      (setf (sqlite-handle obj) (connect (file-name obj)))))

(defmethod finalize-hystory ((obj hystory-data))
  (disconnect (sqlite-handle obj))
  (setf (sqlite-handle obj) nil))