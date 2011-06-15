(in-package :sqlite)

(defun bind-and-execute-non-query (statement &rest parameters)
  (iter (for i from 1)
        (declare (type fixnum i))
        (for prm in parameters)
        (bind-parameter statement i prm))
  (step-statement statement)
  (reset-statement statement)
  (clear-statement-bindings statement)
  (values))

(defun bind-and-execute-to-list (statement &rest parameters)
  (iter (for i from 1)
        (declare (type fixnum i))
        (for prn in parameters)
        (bind-parameter statement i prn))
  
  (prog1
      (iter
        (while (step-statement statement))
        (collect (iter (for col from 0 below (the fixnum (sqlite-ffi:sqlite3-column-count (handle statement))))
                       (declare (type fixnum col))
                       (collect (statement-column-value statement col)))))
    (reset-statement statement)
    (clear-statement-bindings statement)))

(defun execute-many-non-query (db query parameters-list)
  (let ((st (prepare-statement db query)))
    (unwind-protect
         (iter (for param in parameters-list)
               (apply #'bind-and-execute-non-query `(,st ,@param)))
      (finalize-statement st)))
  (values))

(defun create-table (db name fields &optional constraints temporary)
  (restart-case
      (if (> (execute-one-row-m-v db "select count(*) from sqlite_master where type = ? and name = ?" "table" name) 0)
          (error "There is the table with name '~a' already" name)
          (execute-non-query db (format nil "create table ~a(~{~a~^, ~})"
                                        name
                                        (append
                                         (loop for x in fields collect (format nil "~{~a~^ ~}" x))
                                         constraints))))
    (drop-table ()
      "Drop table and create again"
      (execute-non-query db (format nil "drop table ~a" name))
      (create-table db name fields constraints))
    (skip ()
      "Do nothing"
      nil)))

(defun drop-index-all (db)
  (iter (for name in (mapcar #'car (execute-to-list db "select name from sqlite_master where type = 'index'")))
        (execute-non-query db (format nil "drop index ~a" name))))

(defun create-index (db table-name field-names)
  (execute-non-query db (format nil "create index if not exists ~a on ~a(~{~a~^, ~})" (gensym "index") table-name field-names)))