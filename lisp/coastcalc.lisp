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

;(defun universal-deal-compute (&key (direction nil direction?) (coast 
                                 
    
                    
      
  
        