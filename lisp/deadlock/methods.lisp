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
    (make-candle hyst start end period-type)))

(defmethod make-candle ((hyst hystory-data) start end period-type)
    (let ((openco (execute-single (hystory-sqlite-handle hyst) "select open from candles where datetime between ? and ? order by datetime asc" start end))
          (closeco (execute-single (hystory-sqlite-handle hyst) "select close from candles where datetime between ? and ? order by datetime desc" start end))
          (highco (execute-single (hystory-sqlite-handle hyst) "select max(high) from candles where datetime between ? and ?" start end))
          (datetime-open (execute-single (hystory-sqlite-handle hyst) "select datetime from candles where datetime between ? and ? order by datetime asc" start end))
          (datetime-close (execute-single (hystory-sqlite-handle hyst) "select datetime from candles where datetime between ? and ? order by datetime desc" start end))
          (lowco (execute-single (hystory-sqlite-handle hyst) "select min(low) from candles where datetime between ? and ?" start end)))
      (if (and (and openco closeco highco lowco)
               (not (member "" (list openco closeco highco lowco))))
          (make-instance 'candle :open openco :close closeco :high highco :low lowco :datetime datetime-open :datetime-close datetime-close :period period-type))))

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

(defmethod hystory-go-next ((hystory hystory-data))
  (with-slots (current-date sqlite-handle) hystory
    (let ((next-date (execute-single sqlite-handle "select min(datetime) from candles where datetime > ? order by datetime asc" current-date)))
      (unless next-date
        (error (make-condition 'end-of-hystory :format-control "this is the end of hystory")))
      (setf current-date next-date))))

(defmethod hystory-get-current-candle ((hystory hystory-data) &optional period)
  (if (not period)
      (make-candle-from-period hystory (hystory-current-date hystory) (hystory-candle-period hystory))
      (cond 
        ((period>= period (hystory-candle-period hystory))
         (make-candle hystory (start-of-the-period (hystory-current-date hystory) period) (hystory-current-date hystory) period))

        (t (error (make-condition 'wrong-period :format-control "period ~a is less than current hystory's candle's period ~a" :format-arguments (list period (hystory-candle-period hystory))))))))
           
             
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

(defun getpsym (per)
  (cond
    ((listp per) (first per))
    ((symbolp per) per)
    ((numberp per) :sec)))

(defun getpnumb (per)
  (cond
    ((listp per) (second per))
    ((symbolp per) 1)
    ((numberp per) per)))

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

(defmethod set-request ((quick quick) (instrument string) (direction symbol) (count fixnum) (price number) &key subaccount overtime on-set on-execute on-overtime)
  (let ((found-instrument (find-if #'(lambda (a) (equal (string-upcase instrument) (string-upcase (instrument-code a)))) (quick-instuments quick))))
    (unless found-instrument
      (error (make-condition 'incorrect-arguments :format-control "there is no ~a in instruments" :format-arguments '(instrument))))
    (set-request quick found-instrument direction count price :subaccount subaccount :overtime overtime :on-set on-set :on-execute on-execute :on-overtime on-overtime)))

(defmethod set-request ((quick quick) (instrument instrument) (direction symbol) (count fixnum) (price number) &key subaccount overtime on-set on-execute on-overtime)
  (with-slots (subaccounts instruments subaccount-instrument requests) quick
    (if (not subaccount)
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
          (set-request quick instrument direction count price :subaccount sbc :overtime overtime :on-set on-set :on-execute on-execute :on-overtime on-overtime))
        (progn
          (unless (member instrument instruments)
            (error (make-condition 'incorrect-arguments :format-control "there is no that instrument in the quick: ~a" :format-arguments (list instrument))))
          (unless (member subaccount subaccounts)
            (error (make-condition 'incorrect-arguments :format-control "there is no that subaccount in the quick: ~a" :format-arguments (list subaccount))))
          (let ((new-request (make-instance 'request :instrument instrument :direction direction :count count :price price :subaccount subaccount :ttl overtime :set-callback on-set :execute-callback on-execute :overtime-callback on-overtime :set-date (candle-datetime (hystory-get-current-candle (instrument-hystory instrument))))))
            (push new-request requests)
            (when on-set (funcall on-set new-request)))))))

(defmethod execute-request ((q quick) (rq request))
  (unless (check-overtime-request q rq)
    (with-slots (positions deals (qlog log)) q
      (with-slots (instrument subaccount direction price count) rq
        (with-slots (hystory sell-go buy-go) instrument
          (when (let ((c (hystory-get-current-candle hystory)))
                  (>= (candle-low c) price (candle-high c))) ;в текущей свече истории была цена заявленная в нашей заявке
            (let ((found-pos (find-if #'(lambda (pos) (eql (open-position-instrument pos) instrument)) positions)))
              (if found-pos
                  (let ((fdir (open-position-direction found-pos))
                        (fcount (open-position-count found-pos)))
                    (if (or (and (eql fdir :long) (eql direction :buy)) (and (eql fdir :short) (eql direction :sell)))
                        (open-deal-in-position q subaccount found-pos direction instrument count price)
                        (progn
                          (open-deal-in-position q subaccount found-pos  direction instrument (min count fcount) price)
                          (when (> count fcount)
                            (open-new-position q subaccount instrument direction (- count fcount) price))))
                    (let ((fun (request-execute-callback rq))
                          (cc (hystory-get-current-candle hystory)))
                      (when fun (funcall fun rq cc))
                      (setf (slot-value rq 'execution-date)
                            (candle-datetime cc))
                      (quick-log-and-finalize-request q rq))
                    t)
                  (progn
                    (open-new-position q instrument direction count price)
                    (setf (slot-value rq 'execution-date)
                          (candle-datetime (hystory-get-current-candle hystory)))
                    (quick-log-and-finalize-request q rq)
                    t)))))))))

(defmethod check-overtime-request ((q quick) (rq request))
  (labels ((overtimed? (rq)
             (let* ((inst (request-instrument rq))
                    (hys (instrument-hystory inst))
                    (cc (hystory-get-current-candle hys))
                    (cd (candle-datetime cc)))
               (> cd (+ (request-set-date rq) (period-to-seconds (request-ttl rq)))))))
    (when (overtimed? rq)
      (let ((fun (request-overtime-callback rq))
            (cc (hystory-get-current-candle (instrument-hystory (request-instrument rq)))))
        (when fun (funcall fun rq cc))
        (when (overtimed? rq)
          (quick-log-and-finalize-request q rq))))))

(defmethod open-deal-in-position ((q quick) (sbc subaccount) (p open-position) (dir symbol) (count fixnum) (price number) &optional commission)
  (let ((inst (open-position-instrument p))
        (dir (case dir
               (:sell :sell)
               (:buy :buy)
               (:long :buy)
               (:short :sell)))
        (cms (* count (or commission 2.04))))
    (let* ((hys (instrument-hystory inst))
           (sell-go (instrument-sell-go inst))
           (buy-go (instrument-buy-go inst))
           (cc (hystory-get-current-candle hys))
           (cd (candle-datetime cc)))
      (let ((new-deal (make-instance 'deal :count count :instrument inst :date cd :price price :direction dir :commission cms)))
        (with-slots (deals money-holds direction) p
          (push new-deal deals)
          (if (or (and (eql direction :long) (eql dir :buy)) (and (eql direction :short) (eql dir :sell)))
              (dotimes (var count)
                (declare (ignore var))
                (push (hold-money sbc (case dir (:sell sell-go) (:buy buy-go))) money-holds))
              (dotimes (var count)
                (declare (ignore var))
                (free-holded-money (car money-holds))
                (setf money-holds (cdr money-holds))))))
      (let ((new-pcount (open-position-count p)))
        (when (= new-pcount 0)
          (quick-log-and-finalize-position q p))))))
      
(defmethod open-new-position ((q quick) (sbc subaccount) (inst instrument) (dir symbol) (count fixnum) (price number) &optional commission)
  (let* ((cms (* count (or commission 2.04)))
         (hys (instrument-hystory inst))
         (cc (hystory-get-current-candle hys))
         (cd (candle-datetime cc))
         (dir (case dir (:buy :buy) (:sell :sell) (:short :sell) (:long :buy)))
                    
         (new-deal (make-instance 'deal :commission cms :direction dir :count count :instrument inst :date cd :price price))
         (holds (loop for a from 1 to count collect
                     (hold-money sbc (case dir
                                       (:buy (instrument-buy-go inst))
                                       (:sell (instrument-sell-go inst))))))
         (new-pos (make-instance 'open-position :direction (case dir (:buy :long) (:sell :short)) :money-holds holds :deals (list new-deal) :open-date cd)))
    (push new-pos (slot-value q 'positions))))

(defmethod quick-log-and-finalize-request :around ((q quick) (rq request))
  (restart-case
      (let ((state (request-state rq))
            (sbc (request-subaccount rq))
            (inst (request-instrument rq))
            (count (request-count rq))
            (price (request-price rq))
            (sd (request-set-date rq)))
        (cond
          ((eql state :awaiting) (error (make-condition 'can-not-finalize :format-control "state of request is :awaiting, can not finalize")))
          ((eql state :overtimed)
           (unless (request-overtime-date rq)
             (restart-case
                 (error (make-condition 'can-not-finalize :format-control "state of request is :overtimed but overtime-date is not set"))
               (get-from-parameters ()
                 (let ((hys (instrument-hystory inst)))
                   (setf (request-overtime-date rq) (candle-datetime (hystory-get-current-candle hys)))
                   (quick-log-and-finalize-request q rq))))))
          ((eql state :executed)
           (unless (request-execution-date eq)
             (restart-case
                 (error (make-condition 'can-not-finalize :format-control "state of request is :executed but execution-date is not set"))
               (get-from-parameters ()
                 (let ((hys (instrument-hystory inst)))
                   (setf (request-execution-date rq) (candle-datetime (hystory-get-current-candle hys)))
                   (quick-log-and-finalize-request q rq))))))
          (t (error (make-condition 'incorrect-request :format-control "state of request is ~a, it can not be that" :format-arguments (list state)))))
        (unless (numberp sd)
          (error (make-condition 'can-not-finalize :format-control "can not finalize request with set-date equal to ~a" :format-arguments (list sd))))
        (when (or (not sbc) (not (member sbc (quick-subaccounts q))))
          (error (make-condition 'can-not-finalize :format-control "can not finalize request with subaccount set to ~a" :format-arguments (list sbc))))
        (when (or (not inst) (not (member inst (quick-instuments q))))
          (error (make-condition 'can-not-finalize :format-control "can not finalize request with instrument set to ~a" :format-arguments (list inst))))
        (when (or (not count) (not (typep count 'fixnum)) (<= count 0))
          (error (make-condition 'incorrect-request :format-control "can not finalize request with count set to ~a" :format-arguments (list count))))
        (when (or (not price) (not (numberp price)) (<= price 0))
          (error (make-condition 'incorrect-request :format-control "can not finalize request with price set to ~a" :format-arguments (list price))))
        (call-next-method q rq))
    (do-nothing () nil)
    (do-anyway () (call-next-method q rq))))

(defmethod quick-log-and-finalize-request ((q quick) (rq request))
  (quick-log-request q rq)
  (quick-finalize-request q rq))

(defmethod quick-log-request ((q quick) (rq request))
  (sqlite-insert-into (quick-log-sqlite-handler (quick-log q))
                      requests
                      (direction (request-direction rq))
                      (instrument (instrument-code (request-instrument rq)))
                      (count (request-count rq))
                      (price (request-price rq))
                      (set_date (request-set-date rq))
                      (execution_date (request-execution-date rq))
                      (overtime_date (request-overtime-date rq))
                      (state (request-state rq))))

(defmethod quick-finalize-request ((q quick) (rq request))
  (with-accessors ((rqs quick-requests)) q
    (setf rqs (remove rq rqs))))
          
        
      
  
                  
;;;;;;;;;;;;;;;
;; quick-log ;;
;;;;;;;;;;;;;;;

(defmethod finalize-quick-log ((quick-log quick-log))
  (disconnect (quick-log-sqlite-handler quick-log)))

;;;;;;;;;;;;;
;; request ;;
;;;;;;;;;;;;;

      
    
    
  
  