

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

(defun form-62-create-data-table (table-name data-table-name field-names idmu-list)
  (concatenate 'string "create table " table-name
               " as select extract (year from t1.sdate) as year, mt.mu_tum as lpu_name, t1.rowno as row_number, "
               (format nil "~{~a~^, ~}" (mapcar #'(lambda (a)
                                                    (format nil "sum(t1.~a) as ~a" a a)) field-names))
               " from " data-table-name " t1, mu_tum mt where t1.idmu = mt.idmu_tum and ("
               (format nil "~{~a~^ or ~}" (mapcar #'(lambda (a)
                                                      (format nil "(t1.~a is not null and t1.~a <> 0)" a a)) field-names))
               ") and ("
               (format nil "~{~a~^ or ~}" (mapcar #'(lambda (a)
                                                      (format nil "t1.idmu = ~a" a)) idmu-list))
               ") group by extract(year from t1.sdate), mt.mu_tum, t1.rowno order by extract(year from t1.sdate), mt.mu_tum, t1.rowno;"))

(defun form-62-create-data-tables (tslist)
  (mapcar #'(lambda (a)
              (form-62-create-data-table (format nil "f62t~a" (first a)) (format nil "setable~a" (second a)) (loop for fld from 1 to (third a) collect (format nil "field_~a" fld)) *idmus*)) tslist))

(defun form-62-create-summ-table (table-name data-table-name field-names idmu-list)
  (concatenate 'string "create table " table-name "_svd"
               " as select extract (year from t1.sdate) as year, t1.rowno as row_number, "
               (format nil "~{~a~^, ~}" (mapcar #'(lambda (a)
                                                    (format nil "sum(t1.~a) as ~a" a a)) field-names))
               " from " data-table-name " t1, mu_tum mt where t1.idmu = mt.idmu_tum and ("
               (format nil "~{~a~^ or ~}" (mapcar #'(lambda (a)
                                                      (format nil "(t1.~a is not null and t1.~a <> 0)" a a)) field-names))
               ") and ("
               (format nil "~{~a~^ or ~}" (mapcar #'(lambda (a)
                                                      (format nil "t1.idmu = ~a" a)) idmu-list))
               ") group by extract(year from t1.sdate), t1.rowno order by extract(year from t1.sdate), t1.rowno;"))


(defun form-62-create-summ-tables (tslist)
  (mapcar #'(lambda (a)
              (form-62-create-summ-table (format nil "f62t~a" (first a)) (format nil "setable~a" (second a)) (loop for fld from 1 to (third a) collect (format nil "field_~a" fld)) *idmus*)) tslist))


(defun gen-days (days sufix)
  (loop for a in days collect (concatenate 'string (format nil "~d." a) sufix )))

(defun gen-dlo-prescriptions (days)
  (concatenate 'string
               "select t1.create_date, count(*) as count from prescriptions t1, doctors t2, polyclinics t3 where t1.doctor_id = t2.id and t2.polyclinic_id = t3.id and ("
               (format nil "~{~a~^ or ~}" (loop for a in days collect (format nil "t1.create_date = '~a'" a)))
               ") group by t1.create_date;"))

(defun gen-max-for-field (table-name field-names)
  (loop for field from (car field-names) to (cadr field-names) collect
       (format nil "select * from ~a where FIELD_~a = (select max(FIELD_~a) from ~a) and FIELD_~a is not null and FIELD_~a <> 0;"
               table-name field field table-name field field)))

(defun write-list-to-file (filename list)
  (with-open-file (fout filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (loop for line in list do
         (write-line line fout))))

(defun select-all-max (table-name field-names)
  (format nil "select ~{~a ~} from ~a;" (loop for name from (car field-names) to (cadr field-names) collect
                                             (format nil "max(FIELD_~a)" name)) table-name))

(defun massive-update (table-name fields)
  (format nil "update ~a set ~{~a~^, ~}" table-name (mapcar (lambda (a) (format nil "~a = ~a" a a)) fields)))