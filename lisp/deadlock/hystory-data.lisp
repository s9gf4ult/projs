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
(defgeneric make-candle-from-period (hystory datetime period))

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
    (make-candle-from-period hystory back-datetime period-type)))
        
(defmethod make-candle-from-period ((hyst hystory-data) datetime period-type)
  (let ((start (start-of-the-period period-type))
        (end (end-of-the-period period-type)))
    (let ((openco (execute-single (sqlite-handle hyst) "select open from candles where datetime >= ? order by datetime asc" start))
          (closeco (execute-single (sqlite-handle hyst) "select close from candles where datetime <= ? order by datetime desc" end))
          (highco (execute-single (sqlite-handle hyst) "select max(high) from candles where datetime between ? and ?" start end))
          (lowco (execute-single (sqlite-handle hyst) "select min(low) from candles where datetime between ? and ?" start end)))
      (if (and (and openco closeco highco lowco)
               (not (member "" (list openco closeco highco lowco))))
          (make-instance 'candle :open openco :close closeco :high highco :low lowco :datetime start :period period-type)))))
        
  
