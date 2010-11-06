(in-package :deadlock)


         
(defclass strategy () ())

(defgeneric check-strategy (strategy account historydata))

(defmethod check-strategy ((strategy strategy) (account account) (hystory hystory-data))
  (let ((position (make-instance 'trade-position)))
    (with-candles hystory candle ()
                  (if (opened? position)
                      (try-close-position strategy account hystory position candle)
                      (try-open-position strategy account hystory position candle)))))

