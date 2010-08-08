

(defun roll-out-format (table-name field-names row-names)
  (concatenate 'string "select "
               (format nil "~{~a~^, ~}"
                       (loop for tbl from 1 to (length row-names) append
                            (loop for fld in field-names collect (format nil "t~a.~a as ~a_~a" tbl fld fld (elt row-names (- tbl 1))))))
               " from "
               (format nil "~{~a~^, ~}" (loop for tbl from 1 to (length row-names) collect (format nil "~a t~a" table-name tbl)))
               " where "
               (format nil "~{~a~^ and ~}"
                       (list (format nil "~{~a~^ and ~}"
                                     (loop for tbl from 2 to (length row-names) append
                                          (loop for fld in (list "sdate" "edate" "idmu") collect (format nil "t1.~a=t~a.~a" fld tbl fld))))
                             (format nil "~{~a~^ and ~}"
                                     (loop for tbl from 1 to (length row-names) and crow in row-names collect (format nil "t~a.rowno=~a" tbl crow)))))))

(defun form62-table (table-name fields)
  (concatenate 'string
               "select "
               "extract(year from t1.sdate) as year, tt.mu_tum as lpu_name, t1.rowno as row_number, "
               (format nil "~{t1.~a~^, ~}" fields)
               " from " table-name " t1, mu_tum tt"
               " where t1.idmu = tt.idmu_tum and t1.idmu <> 66666 and "
               "("
               (format nil "~{~a~^ or ~}" (mapcar #'(lambda (a) (format nil "(~a is not null and ~a <> 0)" a a)) fields))
               ")"
               " order by t1.sdate, t1.idmu, t1.rowno;"))

(defun form62-table-simple (table-name count-fields)
  (form62-table table-name (mapcar #'(lambda (a) (format nil "field_~a" a)) (loop for a from 1 to count-fields collect a))))

(defun form62-super-simple (setable-number count-fields)
  (write-tmp-file (format nil "setable~a.sql" setable-number) (form62-table-simple (format nil "setable~a" setable-number) count-fields)))

(defun write-tmp-file (filename line &optional (subdir nil))
  (with-open-file (fout (make-pathname :directory (append '(:absolute "home" "razor" "tmp") subdir) :name filename) :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (write-line line fout)))


(defun form62-summarize (table-name fields-count)
  (concatenate 'string
               "create table " table-name "svd "
               "as select year, row_number, "
               (format nil "~{~a~^, ~}" (mapcar #'(lambda (a) (format nil "sum(field_~a) as field_~a" a a)) (loop for a from 1 to fields-count collect a)))
               " from " table-name " group by year, row_number order by year, row_number;"))