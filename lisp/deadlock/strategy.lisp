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
  ())

(defmethod try-open-position ((strategy everyday) (account account) (hystory hystory-data) (position trade-position) (candle candle))
  (