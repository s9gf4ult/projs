(in-package :deadlock)

;;;;;;;;;;;;;;;;
;; money-hold ;;
;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;
;; hystory  ;;
;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;
;; candles ;;
;;;;;;;;;;;;;

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
               (let ((supermonth (+ (* 12 year) month (* p-num times) -1)))
                 (multiple-value-bind (di re) (truncate supermonth 12)
                   (let ((month (+ 1 re))
                         (year di))
                     (multiple-value-bind (sec2 min2 hour2 day2 month2 year2) (decode-universal-time (encode-universal-time sec min hour day month year))
                       (if (not (= day2 day))
                           (datetime-add-period (datetime-add-period datetime (list :day (- day2))) period-type :times times)
                           (encode-universal-time sec min hour day month year))))))))
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

;;;;;;;;;;;
;; quick ;;
;;;;;;;;;;;

(defmethod set-request ((quick quick) (instrument instrument) (direction symbol) (count fixnum) (price number) &key (subaccount null) overtime (on-set function) (on-execute function) (on-overtime function))
  (with-slots (subaccounts subaccount-instrument instruments) quick
    (let ((sc-inst-count 0)
          sbc)
      (dolist (sc-inst subaccount-instrument)
        (when (eql (second sc-inst) instrument)
          (incf sc-inst-count)
          (setf sbc (first sc-inst))))
      (unless (= 1 sc-inst-count)
        (error (make-condition 'incorrect-arguments :format-control "there is ~a allignments between subaccounts and sepcified instrument, can not chose automaticly" :format-arguments '(sc-inst-count))))
      (unless (member sbc subaccounts)
        (error (make-condition 'incorrect-quick :format-control "at least one subaccount link is broken in the quick's list of allignments subaccount-instrument")))
      (set-request quick instrument direction count price :subaccount sbc :overtime overtime :on-set on-set :on-execute on-execute :on-overtime on-overtime))))

(defmethod set-request ((quick quick) (instrument instrument) (direction symbol) (count fixnum) (price number) &key (subaccount subaccount) overtime (on-set function) (on-execute function) (on-overtime function))
  (with-slots (subaccounts instruments requests) quick
    (unless (member instrument instruments)
      (error (make-condition 'incorrect-arguments :format-control "there is no that instrument in the quick")))
    (unless (member subaccount subaccounts)
      (error (make-condition 'incorrect-arguments :format-control "there is no that subaccount in the quick")))
    (let ((new-request (make-instance 'request :instrument instrument :direction direction :count count :price price :subaccount subaccount :ttl overtime :set-callback on-set :execute-callback on-execute :overtime-callback on-overtime)))
      (push new-request requests)
      (when on-set (funcall on-set new-request)))))
  
  
