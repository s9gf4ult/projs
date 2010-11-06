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

(defmacro sqlite-insert-into (db table &rest pairs)
  `(sqlite:execute-non-query ,db ,(concatenate 'string
                                               (format nil "insert into ~a(" table)
                                               (format nil "~{~a~^, ~}" (mapcar #'first pairs)) ") values ("
                                               (format nil "~{~a~^, ~}" (mapcar #'(lambda (a) "?") pairs)) ")")
                             ,@(mapcar #'second pairs)))
                                               
                                               

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
                      (volume "<VOL>"))))))

(defun convert-datetime (date time)
  (macrolet ((bb (&rest pass)
               `(encode-universal-time ,@(loop for p in pass append
                                              (loop for pp in (cdr p) collect
                                                   (if (cadr pp)
                                                       `(parse-integer (subseq ,(car p) ,(car pp) ,(cadr pp)))
                                                       `(parse-integer (subseq ,(car p) ,(car pp)))))))))
                         
    (bb (time (4)
              (2 4)
              (0 2))
        (date (6)
              (4 6)
              (0 4)))))

(defmacro with-source-iter (var source &body body)
  `(loop for ,var = (get-next ,source) while ,var do
        (progn
          ,@body)))

(defun load-coasts-from-file-to-sqlite (coasts-file sqlite-file)
  (let ((coasts (make-instance 'micex-iter :file-name coasts-file))
        (dbconn (sqlite:connect sqlite-file)))
    (unwind-protect
         (load-coasts-from-source-to-database coasts dbconn)
      (sqlite:disconnect dbconn)
      (finalize-iter coasts))))
           

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
