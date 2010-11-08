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
  ((start-day-period :initform (* 40 60) :initarg :start-day-period :accessor everyday-start-period)))

(defmethod try-open-position ((strategy everyday) (account account) (hystory hystory-data) (position trade-position) (candle candle))
  (let ((ctype (candle-type (back-step-candle candle :day 1))))
    (labels ((try-open-long ()
               (when (and (> (- (candle-datetime candle) (candle-datetime (start-of-the-period candle :day))) (everyday-start-period strategy)) ; если прошло больше 40 минут с начала дня
                          (> (candle-high candle) (reduce-candle-values #'candle-high #'max (start-of-the-period candle :day) (back-step-candle candle :s1)))) ; максимальное значение цены текущей свечи больше всех максимальных значений свечей с начала дня
                 (open-long-position account position candle :backstop (list 'lost-percent 2)))
                   
               )
             (try-open-short ()
               
               ))
      (cond
        ((eq :long ctype) (try-open-long))
        ((eq :short ctype) (try-open-short))
        (t (if (not (try-open-long))
                  (try-open-short)))))))
