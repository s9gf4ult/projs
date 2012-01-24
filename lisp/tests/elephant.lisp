(require '#:clsql)
(require '#:lift)

(defpackage #:something
  (:use #:cl #:clsql #:lift #:alexandria))

(in-package #:something)

(deftestsuite clsql-testsuite ()
  ())

(defmacro measure-times (&body body)
  (with-gensyms (realtime runtime)
    `(let ((,realtime (get-internal-real-time))
           (,runtime (get-internal-run-time)))
       (progn
         ,@body)
       (values (- (get-internal-real-time) ,realtime)
               (- (get-internal-run-time) ,runtime)))))

(defun try-connect ()
  (unless *default-database*
    (connect '("127.0.0.1" "test" "test" "test") :database-type :postgresql)))

(defun try-disconnect ()
  (when *default-database*
    (disconnect)))

(defmacro measure-cyclic-things (exec-times first-form second-form)
  (with-gensyms (collected lng summ each x a b)
    `(let ((,collected (loop for ,x from 1 to ,exec-times
                          collect (list (multiple-value-list (measure-times ,first-form))
                                        (multiple-value-list (measure-times ,second-form))))))
       (let ((,lng 0)
             (,summ '((0 0) (0 0))))
         (dolist (,each ,collected)
           (incf ,lng)
           (setf ,summ (mapcar #'(lambda (,a ,b)
                                   (mapcar #'+ ,a ,b)) ,summ ,each)))
         (mapcar #'(lambda (,a)
                     (mapcar #'(lambda (,b) (float (/ ,b ,lng))) ,a)) ,summ)))))

(def-view-class view-a ()
  ((id :db-kind :key :type integer)
   (name :type (string 256) :accessor name :initform "" :initarg :name :db-kind :key)))

(def-view-class view-b ()
  ((id :db-kind :key :type integer)
   (name :type (string 100) :accessor name :initform "" :initarg :name)
   (value :type integer :accessor value :initform nil :initarg :value :db-constraints (unique))))

(def-view-class view-c ()
  ((id :db-king :key :type integer)
   (view-a :db-kind :join
           :db-info (:join-class view-a
                     :home-key view-a-id
                     :foreign-key view-c-id
                     :set t)
           :accessor view-c-a)))

(def-view-class employee ()
  ((name :type (string 20))
   (company
    :accessor employee-company
    :db-kind :join
    :db-info (:join-class company
              :home-key companyid
              :foreign-key companyid
              :set nil))
   (president
    :reader president
	:db-kind :join
	:db-info (:join-class employee
		      :home-key presidentid
              :foreign-key emplid
              :set nil))
   (manager
    :accessor employee-manager
	:db-kind :join
	:db-info (:join-class employee
	          :home-key managerid
              :foreign-key emplid
              :set nil))))

(def-view-class company()
  ((name :type (string 20))
   (employees
	:reader company-employees
	:db-kind :join
	:db-info (:join-class employee
		      :home-key companyid
		      :foreign-key companyid
              :set t))))




(defun measure-table-creation (exec-times)
  (try-connect)
  (measure-cyclic-things exec-times
                         (progn
                           (create-view-from-class 'view-a)
                           (create-view-from-class 'view-b)
                           (create-view-from-class 'view-c))
                         (progn
                           (drop-view-from-class 'view-c)
                           (drop-view-from-class 'view-b)
                           (drop-view-from-class 'view-a))))
                           