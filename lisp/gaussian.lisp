(require 'cl-ppcre)
(require 'sqlite)
(require 'alexandria)


(defun mean (seq)
  (/ (reduce #'+ seq)
     (length seq)))

(defun dispersion (seq &optional (mean nil))
  (let ((m (or mean
               (mean seq))))
    (mean (map 'vector #'(lambda (a) (sqr (- a m))) seq))))

(defun meandisp (seq)
  (let ((m (mean seq)))
    (values m
            (dispersion seq m))))

(defun make-coasts (datafile ticks)
  (let ((a 0)
        (b 0)
        (con (sqlite:connect datafile)))
    (unwind-protect
         (progn 
           (sqlite:execute-non-query con "create table if not exists coasts (id integer primary key not null, coast float not null, time float not null)")
           (sqlite:with-transaction con
             (loop for aa = (* (alexandria:gaussian-random) 0.1)
                for bb = (* (alexandria:gaussian-random) 0.01)
                for count from 1 to ticks
                do
                  (progn
                    (sqlite:execute-non-query con "insert into coasts(coast, time) values (?, ?)" a b)
                    (setf a (+ a aa)
                          b (+ b (abs bb)))))))
      (sqlite:disconnect con))))

(defclass micex-iter()
  ((file-name  :initarg :file-name :accessor file-name)
   (file-descriptor :initform nil :initarg :file-descriptor :accessor file-descriptor)
   (header :initform (make-hash-table :test #'equal) :initarg :header :accessor header)))

(defmethod shared-initialize :after ((obj micex-iter) slot-names &rest init-args &key)
  (when (file-name obj)
    (setf (file-descriptor obj) (open (file-name obj) :direction :input))
    (let  ((fline (ppcre:split "\ *;\ *" (read-line (file-descriptor obj) nil))))
      (loop for a from 0 to (- (length fline) 1) do
           (setf (gethash (elt fline a) (header obj)) a)))))

(defgeneric finalize-iter (iter))
(defmethod finalize-iter ((iter micex-iter))
  (close (file-descriptor iter)))

(defgeneric get-next (iter))
(defmethod get-next ((iter micex-iter))
  (let ((line (read-line (file-descriptor iter) nil)))
    (when line
      (let ((splited (ppcre:split "\ *;\ *" line))
            (ret (make-hash-table :test #'equal)))
        (loop for key being the hash-keys of (header iter) do
             (setf (gethash key ret) (elt splited (gethash key (header iter)))))
        ret))))
      
  
      

  

(defun load-coasts-from-source (source outbase)
    (sqlite:execute-non-query outbase "create table if not exists candles (id integer primary key not null, period iteger, datetime integer, open float, close float, high float, low float, volume float, unique(datetime))")
    (with-source-iter el source
                      (sqlite:execute-non-query outbase "insert into candles (period, datetime, open, close, high, low, volume) values (?,?,?,?,?,?,?)" (gethash "<PER>" el) (convert-datetime (gethash "<DATE>" el) (gethash "<TIME>" el)) (gethash "<OPEN>" el) (gethash "<CLOSE>"el ) (gethash "<HIGH>" el) (gethash "<LOW>" el) (gethash "<VOL>" el))))

    
         
         
  



;; (defun make-money (datasource start-money strategy)
;;   (let ((min-time (+ 10 (sqlite:execute-single datasource "select min(time) from coasts")))
;;         (max-time (sqlite:execute-single datasource "select max(time) from coasts"))
;;         (deposit (make-deposit)))
;;     (loop for time from min-time to max-time by 0.1 do
;;          (when 
