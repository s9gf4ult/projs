(in-package :deadlock)

(defmacro with-candles (hystory candle period &body body)
  (let ((hyst (gensym))
        (open (gensym))
        (close (gensym))
        (datetime (gensym))
        (high (gensym))
        (low (gensym))
        (volume (gensym)))
    `(let ((,hyst (hystory-sqlite-handle ,hystory)))
       (with-sqlite-select ,hyst ((,datetime "datetime") (,open "open") (,close "close") (,high "high") (,low "low") (,volume "volume")) ,(if period
                                                                            "select * from candles where datetime between ? and ?"
                                                                            "select * from candles") ,period
         (let ((,candle (make-instance 'candle :datetime ,datetime :open ,open :close ,close :high ,high :low ,low :volume ,volume)))
               ,@body)))))
     

(defclass hystory-data ()
  ((sqlite-handle :initform nil :initarg :sqlite-handle :accessor hystory-sqlite-handle)
   (file-name :initform nil :initarg :file-name :reader hystory-file-name)
   (candle-period :initform :min :initarg :candle-period :reader hystory-candle-period)))

(defgeneric finalize-hystory (hyst))
(defgeneric back-step-candle (hystory candle period-type &optional steps))
(defgeneric make-candle-from-period (hystory datetime period))
(defgeneric back-step-existing-candle (hystory candle period-type &optional steps)
  (:documentation "вернет предыдущую суцествующую свечу, тобиш в которой проходили сделки(не было выходных проходили сделки и прочее"))

(defmethod shared-initialize :after ((obj hystory-data) slot-names &rest initarts &key)
  (if (not (hystory-sqlite-handle obj))
      (setf (hystory-sqlite-handle obj) (connect (hystory-file-name obj)))))

(defmethod finalize-hystory ((obj hystory-data))
  (disconnect (hystory-sqlite-handle obj))
  (setf (hystory-sqlite-handle obj) nil))

(defmethod back-step-candle ((hystory hystory-data) (datetime number) period-type &optional steps)
  (if (not (period>= period-type (hystory-candle-period hystory)))
      (error "candle period has type ~a but period-type is ~a and it less" (hystory-candle-period hystory) period-type))
  (let* ((steps (or steps
                    1))
         (back-datetime (datetime-add-period datetime (neg-period period-type) :times steps)))
    (make-candle-from-period hystory back-datetime period-type)))

(defmethod back-step-candle ((hyst hystory-data) (candle candle) period-type &optional steps)
  (back-step-candle hyst (candle-datetime candle) period-type steps))
        
(defmethod make-candle-from-period ((hyst hystory-data) datetime period-type)
  (let ((start (start-of-the-period datetime period-type))
        (end (end-of-the-period datetime period-type)))
    (let ((openco (execute-single (hystory-sqlite-handle hyst) "select open from candles where datetime >= ? order by datetime asc" start))
          (closeco (execute-single (hystory-sqlite-handle hyst) "select close from candles where datetime <= ? order by datetime desc" end))
          (highco (execute-single (hystory-sqlite-handle hyst) "select max(high) from candles where datetime between ? and ?" start end))
          (datetime-open (execute-single (hystory-sqlite-handle hyst) "select datetime from candles where datetime >= ? order by datetime asc" start))
          (datetime-close (execute-single (hystory-sqlite-handle hyst) "select datetime from candles where datetime <= ? order by datetime desc" end))
          (lowco (execute-single (hystory-sqlite-handle hyst) "select min(low) from candles where datetime between ? and ?" start end)))
      (if (and (and openco closeco highco lowco)
               (not (member "" (list openco closeco highco lowco))))
          (make-instance 'candle :open openco :close closeco :high highco :low lowco :datetime datetime-open :datetime-close datetime-close :period period-type)))))

(defmethod back-step-existing-candle ((hyst hystory-data) datetime period-type &optional steps)
  (let ((steps (or steps
                   1))
        last)
    (iter (for back-counter from 1)
          (if (equal 0 steps)
              (return last)
              (let ((elt (back-step-candle hyst datetime period-type back-counter)))
                (if elt
                    (setf last elt
                          steps (- steps 1))))))))
