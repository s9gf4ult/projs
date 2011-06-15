(in-package :kladr)

(defvar *db* nil)

(defun kladr-open-database (filename)
  (restart-case
      (if *db*
          (error "database already opened")
          (progn
            (setf *db* (sqlite:connect filename))
            (execute-non-query *db* "pragma foreign_keys=on")))
    (disconnect ()
      (kladr-close-database)
      (kladr-open-database filename))
    (abort () nil)))

(defun kladr-close-database ()
  (when *db*
    (sqlite:disconnect *db*)
    (setf *db* nil)))

(defun comma-separated (seplist)
  (format nil "~{~a~^, ~}" seplist))

(defun to-text (textlist)
  (format nil "~{~a~^ ~}" textlist))

(defun fields-decl (fields-list)
  (comma-separated (mapcar #'to-text fields-list)))

(defun kladr-create-objects ()
  (let ((table-fields '((id integer primary key not null)
                        (name text not null)
                        (short_id integer not null)
                        (code text not null)
                        (gninmb text text)
                        (uno text)
                        (ocatd text)
                        (status text)
                        (region_code integer not null)    ;2 
                        (distinct_code integer not null)  ;3 
                        (city_code integer not null)      ;3
                        (town_code integer not null)      ;3
                        (street_code integer)             ;4
                        (actuality_code integer not null)))) ;2
    (kladr-create-short-names)
    (sqlite::create-table *db* 'kladr_objects table-fields '("unique(region_code, distinct_code, city_code, town_code, street_code, actuality_code)"
                                                              "unique(code)"
                                                              "foreign key (short_id) references short_names(id)"))
    (sqlite::drop-index-all *db*)
    (sqlite::create-index *db* 'kladr '(socr))
    (execute-non-query *db* (format nil "insert into kladr_objects(~{~^, ~})
select k.name, s.id, k.code, k.gninmb, k.uno, k.ocatd, k.status, (substr(k.code, 1, 2) + 0), (substr(k.code, 3, 3) + 0), (substr(k.code, 6, 3) + 0), (substr(k.code, 9, 3) + 0), (substr(k.code, 12, 2) + 0)
from kladr k inner join shot_names s on k.socr = s.name"
                                    '(name short_id code gninmb uno ocatd status region_code distinct_code city_code town_code actuality_code)))
    (execute-non-query *db* (format nil "insert into kladr_objects(~{~^, ~})
select st.name, s.id, st.code, st.gninmb, st.uno, st.ocatd, (substr(st.code, 1, 2) + 0), (substr(st.code, 3, 3) + 0), (substr(st.code, 6, 3) + 0), (substr(st.code, 9, 3) + 0), (substr(st.code, 12, 4) + 0), (substr(st.code, 16, 2) + 0) from street st inner join short_names s on st.socr = s.name"
                                    '(name short_id code gninmb uno ocatd region_code distinct_code city_code town_code street_code actuality_code)))))



    
    
                                                              
    