(defparameter *micex-fixed-commission* 0)
(defparameter *micex-percentage-commission* (rationalize (/ 0.054 100)))
(defparameter *micex-stepback/backstop-amount* 1)
(defparameter *micex-safe-loss-percent* (rationalize (/ 2 100))) ;два роцента от сделки
(defparameter *micex-stepback-multiplicator* 1)


(defun genhashes (columns rows data)
  (let ((ret (make-hash-table)))
    (loop for row in rows and rowdata in data do
         (setf (gethash row ret) (let ((rowhash (make-hash-table)))
                                   (loop for col in columns and coldata in rowdata do
                                        (setf (gethash col rowhash) coldata))
                                   rowhash)))
    ret))

(defun calculate-months (start-money year-percent months month-moneyup)
  (let ((month-percent (expt (+ 1 (/ year-percent 100)) 1/12))
        (accum start-money))
    (dotimes (m months accum)
      (setf accum (+ (* accum (if (> m 0) month-percent 1)) month-moneyup)))))

(defun calculate-real-year-percent (start-money year-percent months month-moneyup)
  (let ((res (calculate-months start-money year-percent months month-moneyup))
        (mulated (+ start-money (* months month-moneyup)))
        (years (truncate (/ months 12))))
    (let ((y-multi (expt (/ res mulated) (/ 1 years))))
      (* 100 (- y-multi 1)))))
    
         
(defun micex-calculate (open direction candle-size &rest args &key  volume count)
  (declare (type symbol direction)
           (type number open candle-size))
  (setf direction (normalize-direction direction))
  (when volume
    (setf count (if count
                    (min (truncate count) (truncate (/ volume open)))
                    (truncate (/ volume open)))))
  (when count
    (setf count (truncate count))
    (setf volume (rationalize (* count open))))
  
  
  (unless (and volume count)
    (error "you must set volume or count at least"))
  (unless (and (> open 0)
               (> count 0)
               (> volume 0)
               (> candle-size 0))
    (error "there is some wrong value here open=~a, count=~a, volume=~a, candle-size=~a" open count volume candle-size))
  (setf candle-size (rationalize candle-size))

  (symbol-macrolet ((takeprof (case direction
                                (:l (+ open takeprofit-stepback lossless))
                                (:s (- open takeprofit-stepback lossless))))
                    (bckstp (case direction
                              (:l (- open backstop-diff))
                              (:s (+ open backstop-diff))))
                    (result `(:open ,(float open) :count ,count :backstop ,(float backstop) :takeprofit ,(float takeprofit) :takeprofit-stepback ,(float takeprofit-stepback) :backstop-loss ,(float (micex-calculate-net :open open :close backstop :count count :direction direction)))))

    (let* ((lossless (case direction
                       (:l (- (lossless-coast open count direction :fixed *micex-fixed-commission* :percentage *micex-percentage-commission*) open))
                       (:s (- open (lossless-coast open count direction :fixed *micex-fixed-commission* :percentage *micex-percentage-commission*)))))
           
           (takeprofit-stepback (* *micex-stepback-multiplicator* candle-size))
           (takeprofit takeprof)
           (backstop-diff (/ takeprofit-stepback *micex-stepback/backstop-amount*))
           (backstop bckstp))
      
      (let ((ls (micex-calculate-net :open open :close backstop :direction direction :count count))
            (vl (* *micex-safe-loss-percent* volume)))
        (if (< ls vl)
            result
            (let* ((backstop-diff (/ (* *micex-safe-loss-percent* volume) count))
                   (backstop (let ((safe-coast (lossless-coast open 14 direction :percentage *micex-percentage-commission*)))
                               (case direction
                                 (:l (- safe-coast backstop-diff))
                                 (:s (+ safe-coast backstop-diff)))))
                   (takeprofit-stepback (* *micex-stepback/backstop-amount* backstop-diff))
                   (takeprofit takeprof))
              result))))))
            
                                                                              
                                                                          
(defun lossless-coast (open count direction &key (fixed 0) (percentage 0))
  (declare (type number open count)
           (type symbol direction))
  (unless (and (> count 0) (> open 0))
    (error "count and open must be greater theat 0"))
  (setf direction (when direction (normalize-direction direction)))
  (/ (+ (* 2 fixed )
        (* count open (case direction
                        (:s (- percentage 1))
                        (:l (+ percentage 1)))))
     (* count (case direction
                (:s (* -1 (+ 1 percentage)))
                (:l (- 1 percentage))))))

(defun avg (&rest args)
  (let ((conc (reduce #'append (mapcar #'(lambda (a) (if (listp a) a (list a))) args))))
    (/ (reduce #'+ conc)
       (list-length conc))))

(defun calculate-commission (&key open-volume volume count open close (fixed 0) (percentage 0) presets)
  (when presets
    (case presets
      (:micex (setf fixed *micex-fixed-commission*
                    percentage *micex-percentage-commission*))
      (otherwise (error "I dont know what is the ~a in :presets" presets))))
  (cond
    ((and open-volume volume count open close) (error "you can not specify :count :open :close :open-volume and :volume at same call"))
    ((and open-volume count) (error "you can not specify open-volume and count at same call"))
    ((and volume count) (error "you can not specify volume and count at same call"))
    ((and volume (or open close)) (error "you can not speciry volume and (or open close) at same call"))
    ((and open-volume open close)
     (progn
       (setf count (truncate (/ open-volume open)))
       (setf volume (* count (+ open close)))))
    ((and count open close)
     (setf volume (* count (+ open close))))
    (volume t)
    (t (error "you has specified strange options")))
  (unless (and (>= volume 0) (or (not count) (>= count 0))
               (or (not open-volume) (>= open-volume 0))
               (or (not open) (>= open 0))
               (or (not close) (>= close 0)))
    (error "one of values is not > 0 volume=~a, count=~a, open-volume=~a, open=~a, close=~a" volume count open-volume open close))
  (if (> volume 0)
      (+ (* 2 fixed)
         (* volume percentage))
      0))

(defun normalize-direction (direction)
  (case direction
    ((:l :long :buy :b) :l)
    ((:s :short :sell) :s)
    (otherwise (error "direction can not be ~a") direction)))

(defun calculate-gross (&key buy sell open close direction count)
  (setf direction (when direction (normalize-direction direction)))
  (cond
    ((and buy sell count (or open close direction)) (error "you can not give open close or direction with sell and buy parameters"))
    ((and open close direction count (or buy sell)) (error "you can not give sell or buy parameters with open and close at same call-"))
    ((and open close direction count)
     (case direction
       (:l (setf buy open
                 sell close))
       (:s (setf buy close
                 sell open))))
    ((and buy sell count) t)
    (t (error "not enought parameters")))
  (* (- sell buy) count))
  
(defun micex-calculate-net (&rest args &key buy sell open close direction count)
  (- (apply #'calculate-gross args)
     (calculate-commission :volume (* count (cond
                                              ((and open close) (+ open close))
                                              ((and buy sell) (+ buy sell))))
                           :fixed *micex-fixed-commission* :percentage *micex-percentage-commission*)))

(defun micex-calculate-net-percent (&rest args &key buy sell open close direction count)
  (* (/ (apply #'micex-calculate-net args) (* count (cond
                                                      ((and buy sell) (+ buy sell))
                                                      ((and open close) (+ open close))))) 100))

   
(require 'lift)

(lift:deftestsuite coastcalc ()
  ())

(lift:addtest (coastcalc) lossless-coast-long-test
              (lift:ensure-random-cases 1000 ()
                (let* ((direction (if (> (random 1.0) 0.5) :l :s))
                       (fix (rationalize (max 0.000001 (random 0.5))))
                       (per (rationalize (max 0.00001 (random 0.01))))
                       (open (rationalize (max 1 (random 500.0))))
                       (count (max 1 (truncate (random 1000))))
                       (close (lossless-coast open count direction :fixed fix :percentage per)))
                  (= (+ (* 2 fix)
                        (* open per count)
                        (* close per count))
                     (* count
                        (case direction
                          (:s (- open close))
                          (:l (- close open))))))))

(lift:addtest (coastcalc) lossless-coast-2
              (let* ((open 100)
                     (close (lossless-coast open 10 :l :fixed 0 :percentage 0)))
                (lift:ensure-same open close)))

(lift:addtest (coastcalc) lossless-coast-3
              (let* ((open 423)
                     (close (lossless-coast open 254 :s :fixed 0 :percentage 0)))
                (lift:ensure-same open close)))

(lift:addtest (coastcalc) lossless-coast-4
              (lift:ensure
               (< (- (lossless-coast 171 (truncate (/ 1600 171)) :l) 171) 3)))

(lift:addtest (coastcalc) lossless-coast-5
              (lift:ensure
               (< (- 171 (lossless-coast 171 (truncate (/ 1600 171)) :s)) 3)))
                                       
(macrolet ((ensure-errors (&rest pairs)
             `(progn
                ,@(loop for pair in pairs collect
                       `(lift:addtest (coastcalc)
                          ,(car pair)
                          (lift:ensure-error ,(cadr pair)))))))
  (ensure-errors
   (lossless-coast-1 (lossless-coast 0 10 :l))
   (lossless-coast-2 (lossless-coast 10 0 :l))
   (lossless-coast-3 (lossless-coast 10 10 43))
   (micex-calculate-1 (micex-calculate 0 :l -1 :volume 100))
   (micex-calculate-2 (micex-calculate 10 :noone 10 :volume 34))
   (micex-calculate-3 (micex-calculate 100 :long 10 :volume -10))
   (micex-calculate-4 (micex-calculate 1000 :l))
   (micex-calculate-5 (micex-calculate 100 :l 10 :volume 34))
   (calculate-commission-1 (calculate-commission :open 10 :close 11 :volume 1100))
   (calculate-commission-2 (calculate-commission :open 34 :count 23 :open-volume 16))
   (calculate-commission-3 (calculate-commission :open-volume 234 :close 13 :count 14))
   (calculate-commission-4 (calculate-commission :count 10 :open 171 :close 173 :volume 45))
   (calculate-commission-9 (calculate-commission :count -10 :open 34 :close 345))
   (calculate-commission-10 (calculate-commission :count 34 :open -10 :close 234))
   (calculate-commission-11 (calculate-commission :volume -2))
   (micex-calculate-net-1 (micex-calculate-net :buy 10 :sell 11 :count 2 :direction :l))
   (micex-calculate-net-2 (micex-calculate-net :buy 10 :sell 34))
   (micex-calculate-net-3 (micex-calculate-net :open 34 :close 66 :count 2))
   (micex-calculate-net-4 (micex-calculate-net :open 34 :sell 89 :count 4 :direction :l))
   (micex-calculate-net-5 (micex-calculate-net :buy 34 :sell 35 :count 100 :direction 34))
   (normalize-direction-1 (normalize-direction 23))))

;; проверка положительности очевидно положительной сделки
(lift:addtest (coastcalc)
  micex-calculate-net-6
  (lift:ensure-different (let ((*micex-fixed-commission* 0)
                               (*micex-percentage-commission* (/ 0.054 100)))
                           (micex-calculate-net :buy 10 :sell 10 :count 3))
                         0 :test #'>=))

;; проверка функции цены безубытка и функции вычисления нет. Нет должен быть равен нулю в цене безубытка
(lift:addtest (coastcalc)
  ensure-lossless
  (lift:ensure-random-cases 1000 ()
    (let* ((open (rationalize (max 0.1 (random 100000.0))))
           (count (truncate (max 1 (random 100000))))
           (direction (if (> (random 1.0) 0.5) :l :s))
           (close (lossless-coast open count direction)))
      (= 0
         (micex-calculate-net :open open :close close :count count :direction direction)))))

;; очевидный тест что коммиссия равна нулю при нулевом объеме и нулевой фиксированной части
(lift:addtest (coastcalc)
  calculate-commission-5
  (lift:ensure-same
   0
   (calculate-commission :count 0 :open 10 :close 34 :fixed 0 :percentage 23)))

;; очевидный тест на то что коммиссия равна нулю при нулевом объеме
(lift:addtest (coastcalc)
  calculate-commission-10
  (lift:ensure-same
   0
   (calculate-commission :count 0 :open 10 :close 11 :fixed 1.0 :percentage 0.02)))

;; очевидный тест на то что коммиссия равна нулю при нулевых частях
(lift:addtest (coastcalc)
  calculate-commission-6
  (lift:ensure-same
   0
   (calculate-commission :count 10 :open 3 :close 45 :fixed 0 :percentage 0)))

;;очевидный тест на то, что коммиссия при нулевой процентной части равна двойной фиксированной части
(lift:addtest (coastcalc)
  calculate-commission-7
  (let ((a (rationalize (random 10.0))))
    (lift:ensure-same
     (* 2 a)
     (calculate-commission :volume 234 :fixed a  :percentage 0))))

;;очевидный тест на то, что коммиссия при нулевой фиксированной части равна объем на проценты
(lift:addtest (coastcalc)
  calculate-commission-8
  (let ((a (rationalize (max 0.001 (random 1.0))))
        (b (max 1 (rationalize (random 10000.0)))))
    (lift:ensure-same
     (* a b)
     (calculate-commission :volume b :fixed 0 :percentage a))))

;;проверяем что закрытие позици по цене тэйкпрофит минус отступ дает нулевой нет
(lift:addtest (coastcalc)
  complex-test-1
  (lift:ensure-random-cases 1000 ()
    (let* ((open (rationalize (max 0.1 (random 10000.0))))
           (count (truncate (max 1 (random 1000))))
           (direction (if (> (random 1.0) 0.5) :l :s))
           (candle-size (rationalize (max (/ open count 10000000) (random (float (/ (* open *micex-stepback/backstop-amount*) 2 *micex-stepback-multiplicator*))))))
           (res (micex-calculate open direction candle-size :count count))
           (close (case direction
                    (:l (- (getf res :takeprofit) (getf res :takeprofit-stepback)))
                    (:s (+ (getf res :takeprofit) (getf res :takeprofit-stepback))))))
      (= 0 (micex-calculate-net :open open :close close :count count :direction direction)))))

;;проверяем что просадка нета по бэкстопу что выдает функция не превышает *micex-safe-loss-percent* от объема торговли
(lift:addtest (coastcalc)
  micex-calculate-7
  (lift:ensure-random-cases 1000 ()
    (let* ((open (rationalize (max 0.1 (random 1000.0))))
           (count (max 1 (truncate (random 1000))))
           (direction (if (> (random 1.0) 0.5) :l :s))
           (size (rationalize (max (/ open count 1000000) (random (float (/ (* open *micex-stepback/backstop-amount*) 2 *micex-stepback-multiplicator*)))))))
      (let* ((res (micex-calculate open direction size :count count))
             (net (micex-calculate-net :open open :close (getf res :backstop) :direction direction :count count))
             (los (* count open *micex-safe-loss-percent*)))
        (and
         (<= (- net) los )
         (<= net 0))))))