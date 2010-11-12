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
(defgeneric back-step-candle (hystory candle period-type &optional steps))

(defmethod shared-initialize :after ((obj hystory-data) slot-names &rest initarts &key)
  (if (not (sqlite-handle obj))
      (setf (sqlite-handle obj) (connect (file-name obj)))))

(defmethod finalize-hystory ((obj hystory-data))
  (disconnect (sqlite-handle obj))
  (setf (sqlite-handle obj) nil))

(defmethod back-step-candle ((hystory hystory-data) (candle candle) period-type &optional steps)
  (if (not (period>= period-type (candle-period candle)))
      (error "candle period has type ~a but period-type is ~a and it less" (candle-period candle) period-type))
  (let* ((steps (or steps
                    1))
         (back-datetime (datetitime-add-period (candle-datetime candle) (neg-period period-type) :times steps)))
    (make-candle-from-period hystory period-type (start-of-the-period back-datetime period-type)
                             (end-of-the-period  back-datetime period-type))))
        
  
  
