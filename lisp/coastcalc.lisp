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

(defun universal-deal-compute (direction open &key volume count backstop takeprofit backstop-diff takeprofit-diff (backstop-percent 0.02 backstop-percent?) (takeprofit-percent 0.03 takeprofit-percent?) (takeprofit-from-backstop-difff 1.5))
  (cond
    (volume
     (setf count (if count
                     (min count (truncate (/ volume open)))
                     (truncate (/ volume open)))))
    (count
     (setf volume (* count open)))
    (t (error "universal-argument-map need :count or :volume parameter")))
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
         
                                        
                        
                                 
    
                    
      
  
        