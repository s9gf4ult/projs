(in-package :deadlock)

(defclass candle()
  ((open :initarg :open :reader candle-open)
   (close :initarg :close :reader candle-close)
   (high :initarg :high :reader candle-high)
   (low :initarg :low :reader candle-low)
   (volume :initarg :volume :reader candle-volume)
   (datetime :initarg :datetime :reader candle-datetime)
   (type :reader candle-type)
   (period :initarg :type :initform :sec :reader candle-period :documentation "period can be one symbol of this (:sec :min :hour :day :week :month :year) or list like this (`symbol' number) where `symbol' is one of above listed. It can be number, in this case it will the same as (:sec number)")))

(defmethod shared-initialize :after ((obj candle) slot-names &rest initargs &key)
  (setf (slot-value obj 'type) (cond
                                 ((> (candle-close obj) (candle-open obj)) :long)
                                 ((< (candle-close obj) (candle-open obj)) :short)
                                 (t :empty))))

(macrolet ((defcompare (&rest pairs)
             `(progn
                ,@(loop for pair in pairs collect
                       `(defun ,(first pair) (p1 p2)
                          (,(second pair) (period-to-seconds p1) (period-to-seconds p2)))))))
  (defcompare
      (period> >)
      (period< <)
    (period= =)
    (period>= >=)
    (period<= <=)))

(defgeneric period-to-seconds (per))

(labels ((getpsym (per)
           (cond
             ((listp per) (first per))
             ((symbolp per) per)
             ((numberp per) :sec)))
         (getpnumb (per)
           (cond
             ((listp per) (second per))
             ((symbolp per) 1)
             ((numberp per) per))))

  (defun datetime-add-period (datetime period-type &key (times 1))
    (let ((p-sym (getpsym period-type))
          (p-num (getpnumb period-type)))
      (cond
        ((member p-sym '(:sec :min :hour :day :week))
         (+ datetime
            (* times
               (period-to-seconds period-type))))
        ((eq p-sym :month)
         (multiple-value-bind (sec min hour day month year) (decode-universal-time datetime)
           (multiple-value-bind (di re) (truncate (+ month (* p-num times)) 12)
             (let ((month (if (> re 0)
                              re
                              (+ 12 re)))
                   (year (+ year di)))
               (multiple-value-bind (sec2 min2 hour2 day2 month2 year2) (decode-universal-time (encode-universal-time sec min hour day month year))
                 (if (not (= day2 day))
                     (datetime-add-period (datetime-add-period datetime (list :day (- day2))) period-type :times times)
                     (encode-universal-time sec min hour day month year)))))))
        ((eq p-sym :year)
         (multiple-value-bind (sec min hour day month year) (decode-universal-time datetime)
           (let ((year (+ year (* p-num times))))
             (encode-universal-time sec min hour day month year)))))))
  
  (defun start-of-the-period (datetime period-type)
    (let ((p-sym (getpsym period-type))
          (p-num (getpnumb period-type)))
      (if (equal 0 p-num)
          (error "period is 0 and can not be used"))
      (cond
        ((equal 1 (abs p-num))
         (multiple-value-bind (sec min hour day month year) (decode-universal-time datetime)
           (case p-sym
             (:sec datetime)
             (:min (encode-universal-time 0 min hour day month year))
             (:hour (encode-universal-time 0 0 hour day month year))
             (:day (encode-universal-time 0 0 0 day month year))
             (:month (encode-universal-time 0 0 0 1 month year))
             (:year (encode-universal-time 0 0 0 1 1 year)))))
        (t
         
        
             
    
  
               
(defgeneric neg-period (per))
(defmethod neg-period ((per list))
  (list (first per) (- (second per))))
(defmethod neg-period ((per symbol))
  (list per -1))
(defmethod neg-period ((per number))
  (list :sec (- per)))
         
  

(defmethod period-to-seconds ((per list))
  (* (second per)
     (case (first per)
       (:sec 1)
       (:min 60)
       (:hour (* 60 60))
       (:day (* 24 60 60))
       (:week (* 7 24 60 60))
       (:month (* 30 24 60 60))
       (:year (* 365 24 60 60)))))

(defmethod period-to-seconds ((per symbol))
  (period-to-seconds (list per 1)))

(defmethod period-to-seconds ((per number))
  number)



