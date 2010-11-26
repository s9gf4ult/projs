(in-package :deadlock)

(defmacro with-sqlite-select (db varlist query parameters &body body)
  (let ((statement (gensym))
        (colcount (gensym))
        (name-number (gensym))
        (i (gensym)))
    `(let ((,statement (prepare-statement ,db ,query)))
       ,@(iter (for i from 1)
               (declare (type fixnum i))
               (for par in parameters)
               (collect `(bind-parameter ,statement ,i ,par)))
       (let ((,colcount (sqlite-ffi:sqlite3-column-count (sqlite::handle ,statement)))
             (,name-number (make-hash-table :test #'equal)))
         (loop for ,i from 0 to (- ,colcount 1) do
              (setf (gethash (sqlite-ffi:sqlite3-column-name (sqlite::handle ,statement) ,i) ,name-number) ,i))
         ,@(loop for var in varlist collect
                `(multiple-value-bind (val has?) (gethash ,(if (listp var)
                                                               (second var)
                                                               (string-downcase (format nil "~a" var))) ,name-number)
                   (declare (ignore val))
                   (if (not has?)
                       (error (make-condition 'simple-condition :format-control "There is no \"~a\" in \"~a\" query results" :format-arguments (list ,(if (listp var)
                                                                                                                                                          (second var)
                                                                                                                                                          (string-downcase (format nil "~a" var))) ,query))))))
         (loop while (step-statement ,statement) do
              (let (,@(loop for var in varlist collect
                           `(,(if (listp var)
                                  (first var)
                                  var) (sqlite:statement-column-value ,statement (gethash ,(if (listp var)
                                                                                               (second var)
                                                                                               (string-downcase (format nil "~a" var))) ,name-number)))))
                ,@body)))
       (sqlite:finalize-statement ,statement))))

(defmacro with-source-iter (var source &body body)
  `(loop for ,var = (get-next ,source) while ,var do
        (progn
          ,@body)))

(defmacro sqlite-insert-into (db table &rest pairs)
  `(sqlite:execute-non-query ,db ,(concatenate 'string
                                               (format nil "insert into ~a(" table)
                                               (format nil "~{~a~^, ~}" (mapcar #'first pairs)) ") values ("
                                               (format nil "~{~a~^, ~}" (mapcar #'(lambda (a) (declare (ignore a)) "?") pairs)) ")")
                             ,@(mapcar #'second pairs)))

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
  (declare (ignore slot-names init-args))
  (when (file-name obj)
    (setf (file-descriptor obj) (open (file-name obj) :direction :input))
    (let  ((fline (ppcre:split "\ *;\ *" (read-line (file-descriptor obj) nil))))
      (loop for a from 0 to (- (length fline) 1) do
           (setf (gethash (elt fline a) (header obj)) a)))))

(defgeneric finalize-iter (iter))
(defgeneric get-next (iter))


(defmethod finalize-iter ((iter micex-iter))
  (close (file-descriptor iter)))

(defmethod get-next ((iter micex-iter))
  (let ((line (read-line (file-descriptor iter) nil)))
    (when line
      (let ((splited (ppcre:split "\ *;\ *" line))
            (ret (make-hash-table :test #'equal)))
        (loop for key being the hash-keys of (header iter) do
             (setf (gethash key ret) (elt splited (gethash key (header iter)))))
        ret))))

                                               
                                               

(defun load-coasts-from-source-to-database (source outbase)
  (sqlite:execute-non-query outbase "create table if not exists candles (id integer primary key not null, period iteger, datetime integer, open float, close float, high float, low float, volume float, unique(datetime))")
  (sqlite:with-transaction outbase
    (with-source-iter el source
      (macrolet ((short-insert (db table &rest pairs)
                   `(sqlite-insert-into ,db ,table ,@(mapcar #'(lambda (a)
                                                               (list (first a)
                                                                     (if (stringp (second a))
                                                                         `(read-from-string (gethash ,(second a) el))
                                                                         (second a)))) pairs))))
        (short-insert outbase candles
                      (period "<PER>")
                      (datetime (convert-datetime (gethash "<DATE>" el) (gethash "<TIME>" el)))
                      (open "<OPEN>")
                      (close "<CLOSE>")
                      (high "<HIGH>")
                      (low "<LOW>")
                      (volume "<VOL>")
                      )))))

(defun convert-datetime (date time)
  (macrolet ((bb (&rest pass)
               `(encode-universal-time ,@(loop for p in pass append
                                              (loop for pp in (cdr p) collect
                                                       `(parse-integer (subseq ,(car p) ,@pp)))))))
                         
    (bb (time (4)
              (2 4)
              (0 2))
        (date (6)
              (4 6)
              (0 4)))))


(defun load-coasts-from-file-to-sqlite (coasts-file sqlite-file)
  (let ((coasts (make-instance 'micex-iter :file-name coasts-file))
        (dbconn (sqlite:connect sqlite-file)))
    (unwind-protect
         (load-coasts-from-source-to-database coasts dbconn)
      (sqlite:disconnect dbconn)
      (finalize-iter coasts))))
           
(defmacro with-candles (hystory candle period-type period &body body)
  (let ((move-up (gensym))
        (move-to (gensym))
        (current-shift (gensym))
        (current-date (gensym))
        (start (gensym))
        (end (gensym))
        (datetime-open (gensym))
        (datetime-close (gensym)))
    `(let (,@(if period
                 `((,datetime-open (let ((,start ,(first period)))
                                       (if (typep ,start 'candle)
                                         (candle-datetime ,start)
                                         ,start)))
                   (,datetime-close (let ((,end ,(second period)))
                                      (if (typep ,end 'candle)
                                          (candle-datetime ,end)
                                          ,end))))
                 `((,datetime-open (execute-single (hystory-sqlite-handle ,hystory) "select min(datetime) from candles"))
                   (,datetime-close (execute-single (hystory-sqlite-handle ,hystory) "select max(datetime) from candles")))))
       (let ((,move-up (start-of-the-period ,datetime-open ,period-type))
             (,move-to (start-of-the-period ,datetime-close ,period-type)))
         (iter (for ,current-shift from 0)
               (declare (type fixnum ,current-shift))
               (let ((,current-date (datetime-add-period ,move-up ,period-type :times ,current-shift)))
                 (if (<= ,current-date ,move-to)
                     (let ((,candle (make-candle-from-period ,hystory ,current-date ,period-type)))
                       (when ,candle
                         ,@body))
                     (return nil))))))))
                           
