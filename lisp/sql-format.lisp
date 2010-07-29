

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
               "extract(year from t1.sdate) as year, tt.mu_tum as lpu_name, "
               (format nil "~{t1.~a~^, ~}" fields)
               " from " table-name " t1, mu_tum tt"
               " where t1.idmu = tt.idmu_tum order by t1.sdate, t1.idmu, t1.rowno"))

(defun write-tmp-file (filename line)
  (with-open-file (fout (make-pathname :directory '(:absolute "home" "razor" "tmp") :name filename) :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (write-line line fout)))
               