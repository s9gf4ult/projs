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
    
(defun compute-deal-parameters (coast count direction &key (back-stop-percent 0.02) (take-profit-percent 3/100 tpp?) (take-profit-from-backstop 1.5 tpfb?) volume)
  (let* ((volume (or volume (* coast count)))
         (bs-diff (/ (* back-stop-percent volume) count))
         (back-stop-coast (if (eq 'l direction)
                              (- coast bs-diff)
                              (+ coast bs-diff)))
         (tp-diff (cond
                    ((or tpp? (and (not tpp?) (not tpfb?))) (/ (* volume take-profit-percent) count))
                    (tpfb? (* bs-diff take-profit-from-backstop))))
         (take-profit-coast (if (eq 'l direction)
                                (+ coast tp-diff)
                                (- coast tp-diff))))
    `(:backstop ,back-stop-coast
                :takeprofit-limit ,take-profit-coast)))

(defun universal-deal-compute (direction open &key volume count backstop takeprofit backstop-diff takeprofit-diff (backstop-percent 0.02 backstop-percent?) (takeprofit-percent 0.03 takeprofit-percent?) (takeprofit-from-backstop-difff 1.5) get-lossless (commission :micex commission?) step-back)
  (cond
    (volume
     (setf count (if count
                     (min count (truncate (/ volume open)))
                     (truncate (/ volume open)))))
    (count
     (setf volume (* count open)))
    (t (error "universal-deal-compute need :count or :volume parameter")))
  (flet ((compute-backstop ()
           (when (not backstop)
             (when (not backstop-diff)
               (setf backstop-diff (/ (* volume backstop-percent) count)))
             (setf backstop (if (eq 'l direction)
                                (- open backstop-diff)
                                (+ open backstop-diff))))
           (setf backstop-diff
                 (or backstop-diff
                     (if (eq 'l direction)
                         (- open backstop)
                         (- backstop open)))))
                                 
         (compute-takeprofit ()
           (when (not takeprofit)
             (when (not takeprofit-diff)
               (setf takeprofit-diff (cond
                                       (takeprofit-from-backstop-difff
                                        (* backstop-diff takeprofit-from-backstop-difff))
                                       (takeprofit-percent
                                        (/ (* volume takeprofit-percent) count)))))
             (setf takeprofit (if (eq 'l direction)
                                  (+ open takeprofit-diff)
                                  (- open takeprofit-diff))))
           (setf takeprofit-diff
                 (or takeprofit-diff
                     (if (eq 'l direction)
                         (- takeprofit open)
                         (- open takeprofit))))))
    (let ((back-first (or backstop backstop-diff backstop-percent?))
          (profit-first (or takeprofit takeprofit-diff takeprofit-percent?)))
      (cond
        ((or back-first (and (not back-first) (not profit-first)))
         (progn
           (compute-backstop)
           (compute-takeprofit)))
        (profit-first
         (progn
           (compute-takeprofit)
           (when (and takeprofit-from-backstop-difff (or takeprofit takeprofit-diff))
             (when (not takeprofit-diff) (setf takeprofit-diff (if (eq 'l direction)
                                                                   (- takeprofit open)
                                                                   (+ open takeprofit))))
             (setf backstop-diff (/ takeprofit-diff takeprofit-from-backstop-difff)))
           (compute-backstop))))))
  `(:open ,open :count ,count :volume ,volume :backstop ,backstop :takeprofit ,takeprofit))
         
(defun micex-calculate (open direction &key backstop volume count min max)
  (declare (type symbol direction)
           (type number open))
  (cond
    ((member direction '(l :l :long :buy)) (setf direction :l))
    ((member direction '(s :s :short :sell)) (setf direction :s))
    (t 
     (error "micex-calculate needs direcion be the member of list (:l :long :buy :s :short :sell)")))
  (when volume
    (setf count (if count
                    (min (truncate count) (truncate (/ volume open)))
                    (truncate (/ volume open)))))
  (when count
    (setf count (truncate count))
    (setf volume (or volume
                     (* count open))))
  (when (and min max)
    (unless (>= min open max)
      (error "the condition (>= min open max) is not true")))
  (unless (and volume count)
    (error "you must set volume or count at least"))
  (unless (and (> open 0)
               (> count 0)
               (> volume 0)
               (or (not min) (> min 0))
               (or (not max) (> max 0)))
    (error "there is some wrong value here open=~a, count=~a, volume=~a, min=~a, max=~a" open count volume min max))
               
  
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
    `(:open ,(float open) :count ,count :backstop ,(float backstop) :takeprofit ,(float takeprofit) :takeprofit-stepback ,(float takeprofit-stepback))))


(defun lossless-coast (open count direction &key (fixed 0.54) (percentage 0.0004))
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
   (micex-calculate-7 (micex-calculate 150 :s :volume 200 :min 100 :max 110))
   (micex-calculate-8 (micex-calculate 100 :l :volume 569 :min 200 :max 100))))
                
                                 