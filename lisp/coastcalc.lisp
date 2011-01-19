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
    
         
(defun micex-calculate (open direction &key backstop volume count min max)
  (declare (type symbol direction)
           (type number open))
  (cond
    ((member direction '(l :l :long :buy :b)) (setf direction :l))
    ((member direction '(s :s :short :sell)) (setf direction :s))
    (t 
     (error "micex-calculate needs direcion be the member of list (:l :long :buy :s :short :sell)")))
  (when volume
    (setf count (if count
                    (min (truncate count) (truncate (/ volume open)))
                    (truncate (/ volume open)))))
  (when count
    (setf count (truncate count))
    (setf volume (* count open)))
  
  ;; (when (and min max)
  ;;   (unless (<= min open max)
  ;;     (error "the condition (>= min open max) is not true")))
  (unless (and volume count)
    (error "you must set volume or count at least"))
  (unless (and (> open 0)
               (> count 0)
               (> volume 0)
               (or (not min) (> min 0))
               (or (not max) (> max 0))
               (or (not backstop) (> backstop 0)))
    (error "there is some wrong value here open=~a, count=~a, volume=~a, min=~a, max=~a, backstop=~a" open count volume min max backstop))
               
  
  (let* ((backstop-diff (if backstop
                            (case direction
                              (:l (/ (- open backstop) count))
                              (:s (/ (- backstop open) count)))
                            (let ((safe (/ (* 0.02 volume) count)))
                              (cond
                                ((and min max) (min safe
                                                    (let ((a (/ (- max min) 3)))
                                                      (if (> a 0) a
                                                          (error "min = ~a max = ~a this is incorrect" min max)))))
                                ((and (not min) (not max)) safe)
                                (t (error "you must set min and max both or no one"))))))
         (backstop (or backstop
                       (case direction
                         (:l (- open backstop-diff))
                         (:s (+ open backstop-diff)))))
         
         (lossless (case direction
                     (:l (- (lossless-coast open count direction :fixed 0 :percentage (rationalize (/ 0.057 100))) open))
                     (:s (- open (lossless-coast open count direction :fixed 0 :percentage (rationalize (/ 0.057 100)))))))
         
         (takeprofit-stepback (/ backstop-diff 2))
              
         (takeprofit (case direction
                       (:l (+ open takeprofit-stepback lossless))
                       (:s (- open takeprofit-stepback lossless)))))
    `(:open ,(float open) :count ,count :backstop ,(float backstop) :takeprofit ,(float takeprofit) :takeprofit-stepback ,(float takeprofit-stepback) :posible-loss ,(float (* count backstop-diff))
            :posible-profit ,(float (let ((c (case direction
                                               (:l (- (- takeprofit takeprofit-stepback) open))
                                               (:s (- open (+ takeprofit takeprofit-stepback))))))
                                      (* c count))))))
                                                                                                                            
                                                                          
(defun lossless-coast (open count direction &key (fixed 0.54) (percentage 0.0004))
  (declare (type number open count)
           (type symbol direction))
  (unless (and (> count 0) (> open 0))
    (error "count and open must be greater theat 0"))
  (unless (member direction '(:l :s))
    (error "direction must be :l or :s"))
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
      (:micex (setf fixed 0
                    percentage (/ 0.054 100)))
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
  (+ (* 2 fixed)
     (* volume percentage)))
  
      

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
   (micex-calculate-1 (micex-calculate 0 :l :volume 100))
   (micex-calculate-2 (micex-calculate 10 :noone :volume 34))
   (micex-calculate-3 (micex-calculate 100 :long :volume -10))
   (micex-calculate-4 (micex-calculate 1000 :l))
   (micex-calculate-5 (micex-calculate 100 :l :count 34 :min 34))
   (micex-calculate-6 (micex-calculate 150 :s :volume 100 :min 100 :max 200))
   (micex-calculate-8 (micex-calculate 100 :l :volume 569 :min 200 :max 100))
   (micex-calculate-9 (micex-calculate 100 :b :volume 150 :backstop -11))
   (calculate-commission-1 (calculate-commission :open 10 :close 11 :volume 1100))
   (calculate-commission-2 (calculate-commission :open 34 :count 23 :open-volume 16))
   (calculate-commission-3 (calculate-commission :open-volume 234 :close 13 :count 14))
   (calculate-commission-4 (calculate-commission :count 10 :open 171 :close 173 :volume 45))
   (calculate-commission-9 (calculate-commission :count -10 :open 34 :close 345))
   (calculate-commission-10 (calculate-commission :count 34 :open -10 :close 234))
   (calculate-commission-11 (calculate-commission :volume -2))))

(lift:addtest (coastcalc)
  calculate-commission-5
  (lift:ensure-same
   0
   (calculate-commission :count 0 :open 10 :close 34 :fixed 0 :percentage 23)))


(lift:addtest (coastcalc)
  calculate-commission-6
  (lift:ensure-same
   0
   (calculate-commission :count 10 :open 3 :close 45 :fixed 0 :percentage 0)))

(lift:addtest (coastcalc)
  calculate-commission-7
  (let ((a (rationalize (random 10.0))))
    (lift:ensure-same
     (* 2 a)
     (calculate-commission :volume 234 :fixed a  :percentage 0))))

(lift:addtest (coastcalc)
  calculate-commission-8
  (let ((a (rationalize (max 0.001 (random 1.0))))
        (b (max 1 (rationalize (random 10000.0)))))
    (lift:ensure-same
     (* a b)
     (calculate-commission :volume b :fixed 0 :percentage a))))
                    