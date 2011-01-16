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
                                (+ open backstop-diff)))))
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
                                  (- open takeprofit-diff))))))
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
                    (min count (truncate (/ volume open)))
                    (truncate (/ volume open)))))
  (when count
    (setf volume (or volume
                     (* count open))))
  (unless (and volume count)
    (error "you must set volume or count at least"))
  
  (let* ((backstop-diff (if backstop
                            (case direction
                              (:l (/ (- open backstop) count))
                              (:s (/ (- backstop open) count)))
                            (let ((safe (/ (* 0.02 volume) count)))
                              (cond
                                ((and min max) (min safe
                                                    (let ((a (- max min)))
                                                      (if (> a 0) a
                                                          (error "min = ~a max = ~a this is incorrect" min max)))))
                                ((and (not min) (not max)) safe)
                                (t (error "you must set min and max both or no one"))))))
         (backstop (or backstop
                       (case direction
                         (:l (- open backstop-diff))
                         (:s (+ open backstop-diff)))))
         (takeprofit-stepback (/ backstop-diff 2))
         (lossless (let ((c (/ (+ (* 2 0.54)
                                  (* count open (case direction
                                                  (:s (- 0.002 1))
                                                  (:l (+ 0.002 1)))))
                               (* count (case direction
                                          (:s (* -1 (+ 1 0.002)))
                                          (:l (- 1 0.002)))))))
                     (case direction
                       (:l (- c open))
                       (:s (- open c)))))
         (takeprofit (case direction
                       (:l (+ open takeprofit-stepback lossless))
                       (:s (- open takeprofit-stepback lossless)))))
    `(:open ,(float open) :backstop ,(float backstop) :takeprofit ,(float takeprofit) :takeprofit-stepback ,(float takeprofit-stepback))))
         
                                      
                     
                                         
    