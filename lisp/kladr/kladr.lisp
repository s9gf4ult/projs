(in-package :kladr)

(defvar *db* nil)

(defun kladr-open-database (filename)
  (restart-case
      (if *db*
          (error "database already opened")
          (setf *db* (sqlite:connect filename)))
    (disconnect ()
      (close-kladr-database)
      (open-kladr-database filename))
    (abort () nil)))

(defun kladr-close-database ()
  (when *db*
    (sqlite:disconnect *db*)
    (setf *db* nil)))


(defun execute-many (db query list-of-arguments)
  (let ((stmt (prepare-statement db query)))
    (iter (for args in list-of-arguments)
          (declare (type list args))
          (iter (for i from 1)
                (declare (type fixnum i))
                (for value in args)
                (bind-parameter stmt i value))
          (step-statement stmt))
    (finalize-statement stmt)
    (values)))
          
