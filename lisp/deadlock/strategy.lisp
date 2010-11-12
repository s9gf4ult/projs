(in-package :deadlock)


         
(defclass strategy () ())

(defgeneric check-strategy (strategy account historydata))
(defgeneric try-close-position (strategy account hystory position candle))
(defgeneric try-open-position (strategy account hystory position candle))

(defmethod check-strategy ((strategy strategy) (account account) (hystory hystory-data))
  (let ((position (make-instance 'trade-position)))
    (with-candles hystory candle ()
                  (if (opened? position)
                      (try-close-position strategy account hystory position candle)
                      (try-open-position strategy account hystory position candle)))))

(defclass everyday (strategy)
  ((start-period :initform (* 40 60) :initarg :start-period :accessor everyday-start-period)
   (backstop-value :initform (list 'percent 2) :initarg :backstop-value :accessor everyday-backstop-value)))

(defmethod try-open-position ((strategy everyday) (account account) (hystory hystory-data) (position trade-position) (candle candle))
  (let ((ctype (candle-type (back-step-candle hystory candle :day))))
    (labels ((try-open-long ()
               (when (> (candle-high candle) (reduce-candle-values #'candle-high #'max (start-of-the-period hystory candle :day) (back-step-candle hystory candle :sec))) ; максимальное значение цены текущей свечи больше всех максимальных значений свечей с начала дня
                 (open-long-position account position candle :backstop (everyday-backstop-value strategy))))
             (try-open-short ()
               (when (< (candle-low candle) (reduce-candle-values #'candle-low #'min (start-of-the-period hystory candle :day) (back-step-candle hystory candle :sec))) 
                 (open-short-position account position candle :backstop (everyday-backstop-value strategy)))))
      (when (> (- (candle-datetime candle) (candle-datetime (start-of-the-period candle :day))) (everyday-start-period strategy))
        (cond
          ((eq :long ctype) (try-open-long))
          ((eq :short ctype) (try-open-short))
          (t (if (not (try-open-long))
                 (try-open-short))))))))
