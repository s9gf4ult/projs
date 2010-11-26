(in-package :deadlock)

(defmethod hold-money ((subaccount subaccount) (count number))
  (make-instance 'money-hold :subaccount subaccount :money count))

(defmethod withdraw-money ((subaccount subaccount) (money number))
  (with-slots (free-money) subaccount
    (if (<= money free-money)
        (setf free-money (- free-money money))
        (error (make-condition 'not-enought-money :need-money money :got-money free-money :format-control "there is not enought money to withdraw")))))

(defmethod free-holded-money ((hold money-hold))
  (with-slots (subaccount money) hold
    (with-slots (free-money) subaccount
      (incf free-money money)
      (setf money 0
            subaccount nil))))

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
          (declare (type fixnum back-counter))
          (if (equal 0 steps)
              (return last)
              (let ((elt (back-step-candle hyst datetime period-type back-counter)))
                (if elt
                    (setf last elt
                          steps (- steps 1))))))))

(defmethod reduce-candle-values ((hys hystory-data) reduce-function map-function &key start end period)
  (let ((start (or start
                   (execute-single (hystory-sqlite-handle hys) "select min(datetime) from candles")))
        (end (or end
                 (execute-single (hystory-sqlite-handle hys) "select max(datetime) from candles")))
        (period (or period :min)))
    
    (let (result)
      (with-candles hys candle period (start end)
        (setf result (if (not result)
                         (funcall map-function candle)
                         (funcall reduce-function result (funcall map-function candle)))))
      result)))
