(in-package :deadlock)

(defclass candle()
  ((open :initform nil :initarg :open :reader candle-open)
   (close :initform nil :initarg :close :reader candle-close)
   (high :initform nil :initarg :high :reader candle-high)
   (low :initform nil  :initarg :low :reader candle-low)
   (volume :initform nil :initarg :volume :reader candle-volume)
   (datetime :initform nil :initarg :datetime :reader candle-datetime)
   (datetime-close :initform nil :initarg :datetime-close :reader candle-datetime-close)
   (type :initform nil :reader candle-type)
   (period :initarg :period :initform :sec :reader candle-period :documentation "period can be one symbol of this (:sec :min :hour :day :week :month :year) or list like this (`symbol' number) where `symbol' is one of above listed. It can be number, in this case it will the same as (:sec number)")))

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
(defgeneric neg-period (per))
(defgeneric print-candle (candle))
(defgeneric datetime-add-period (datetime period &key times))
(defgeneric start-of-the-period (candle-or-datetime period))
(defgeneric end-of-the-period (candle-or-datetime period))


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

  (defmethod datetime-add-period ((candle candle) period &key times)
    (datetime-add-period (candle-datetime candle) period times))

  (defmethod datetime-add-period ((datetime number) period-type &key times)
    "добавляем к датавремя период в формате временного периода
в случае работы с месяцами учитывается что разные месяцы состоят из разного количества дней, так если к дате конца месяца прибавить один месяц, то полученная дата будет концом следующего месяца даже если это будет февраль"
    
    (let ((times (or times 1))
          (p-sym (getpsym period-type))
          (p-num (getpnumb period-type)))
      (if (= 0 times) datetime
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
                 (encode-universal-time sec min hour day month year))))))))
    
    (defmethod start-of-the-period ((datetime number) period-type)
      "вычисляем начало временного периода в котором находиться `datetime'"
      (let ((p-sym (getpsym period-type))
            (p-num (getpnumb period-type)))
        (if (equal 0 p-num)
            (error "period is 0 and can not be used"))
        (cond
          ((equal 1 (abs p-num))
           (multiple-value-bind (sec min hour day month year dow) (decode-universal-time datetime)
             (case p-sym
               (:sec datetime)
               (:min (encode-universal-time 0 min hour day month year))
               (:hour (encode-universal-time 0 0 hour day month year))
               (:day (encode-universal-time 0 0 0 day month year))
               (:week (datetime-add-period (encode-universal-time 0 0 0 day month year) (list :day (- dow))))
               (:month (encode-universal-time 0 0 0 1 month year))
               (:year (encode-universal-time 0 0 0 1 1 year)))))
          (t
           (return-range-of-multiple-period datetime period-type)))))
    
    (defmethod start-of-the-period ((candle candle) period-type)
      (start-of-the-period (candle-datetime candle) period-type))

    (defmethod end-of-the-period ((datetime number) period-type)
      "выделяем конец периода `period-type' в котором `datetime'"
      (let ((p-sym (getpsym period-type))
            (p-num (getpnumb period-type)))
        (if (equal 0 p-num)
            (error "period is 0 and can not be used"))
        (cond
          ((equal 1 (abs p-num)) ;если вычисляем конец периода в одну величину
           (multiple-value-bind (sec min hour day month year dow) (decode-universal-time datetime)
             (case p-sym
               (:sec datetime)
               (:min (encode-universal-time 59 min hour day month year))
               (:hour (encode-universal-time 59 59 hour day month year))
               (:day (encode-universal-time 59 59 23 day month year))
               (:week (datetime-add-period (encode-universal-time 59 59 23 day month year) (list :day (- 6 dow))))
               (:month (multiple-value-bind (sec min hour day month year) (decode-universal-time (datetime-add-period datetime :month))
                         (- (encode-universal-time 59 59 23 1 month year) (* 60 60 24)))) ;вернуть первый день следующего месяца минус один день (тобишь конец предыдущего месяца)
               (:year (encode-universal-time 59 59 23 31 12 year)))))
          (t
           (return-range-of-multiple-period datetime period-type :end)))))

    (defmethod end-of-the-period ((candle candle) period-type)
      (end-of-the-period (candle-datetime candle) period-type))
    
    (defun return-range-of-multiple-period (datetime period-type &optional (start-or-end :start))
      "возвращаем список с началом и концом временного периодка `period-type' в котором находиться `datetime' если первый является составным временным периодом"
      (let ((p-sym (getpsym period-type))
            (p-num (getpnumb period-type)))
        (let ((counting (cond
                          ((member p-sym '(:sec :min)) 60)
                          ((eq :hour p-sym) 24)
                          ((eq :month p-sym) 12))))
          (cond
            ((member p-sym '(:sec :min :hour :month))
             (labels ((p-start (val per)
                        (* per (truncate val per)))
                      (p-end (val per)
                        (+ (p-start val per) (- per 1))))
               (if (not (and (< p-num counting) (= 0 (rem counting p-num))))
                   (error "you want to use ~a of ~a as period type, you can not do that" p-num p-sym)
                   (multiple-value-bind (sec min hour day month year dow) (decode-universal-time datetime)
                     (case p-sym
                       (:sec (if (eq :start start-or-end)
                                 (encode-universal-time (p-start sec p-num) min hour day month year)
                                 (encode-universal-time (p-end sec p-num) min hour day month year)))
                       (:min (if (eq :start start-or-end)
                                 (encode-universal-time 0 (p-start min p-num) hour day month year)
                                 (encode-universal-time 59 (p-end min p-num) hour day month year)))
                       (:hour (if (eq :start start-or-end)
                                  (encode-universal-time 0 0 (p-start hour p-num) day month year)
                                  (encode-universal-time 59 59 (p-end hour p-num) day month year)))
                       (:month (if (eq :start start-or-end)
                                   (encode-universal-time 0 0 0 1 (+ 1 (p-start (- month 1) p-num)) year)
                                   (end-of-the-period (encode-universal-time 0 0 0 1 (+ 1 (p-end (- month 1) p-num)) year) :month))))))))
            
            ))))

    )
    
  
               
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
  per)

(defmethod print-candle ((candle candle))
  (when (candle-datetime candle)
    (multiple-value-bind (sec min hour day month year) (decode-universal-time (candle-datetime candle))
      
      (write-line (format nil "(datetime (sec ~a) (min ~a) (hour ~a) (day ~a) (month ~a) (year ~a))" sec min hour day month year))))
  (when (candle-datetime-close candle)
    (multiple-value-bind (sec min hour day month year) (decode-universal-time (candle-datetime-close candle))
      (write-line (format nil "(datetime-close (sec ~a) (min ~a) (hour ~a) (day ~a) (month ~a) (year ~a))" sec min hour day month year))))
  (macrolet ((short-write (&rest pairs)
               `(progn ,@(loop for pair in pairs collect 
                             `(when (,(first pair) candle)
                                (write-line (format nil ,(format nil "(~a ~~a)" (second pair)) (,(first pair) candle))))))))
    (short-write (candle-open open)
                 (candle-close close)
                 (candle-high high)
                 (candle-low low)
                 (candle-volume volume)
                 (candle-type type)
                 (candle-period period))) nil )

(defmethod print-candle ((candle null))
  (write-line "NIL"))