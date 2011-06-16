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
    (sqlite::drop-index-all *db*)
    (sqlite::create-index *db* 'kladr '(socr))
    (kladr-create-short-names)
    (sqlite::create-table *db* 'kladr_objects table-fields '("unique(region_code, distinct_code, city_code, town_code, street_code, actuality_code)"
                                                              "unique(code)"
                                                              "foreign key (short_id) references short_names(id)"))
    (print "table created")
    (execute-non-query *db* (format nil "insert into kladr_objects(~{~a~^, ~})
select k.name, s.id, k.code, k.gninmb, k.uno, k.ocatd, k.status, (substr(k.code, 1, 2) + 0), (substr(k.code, 3, 3) + 0), (substr(k.code, 6, 3) + 0), (substr(k.code, 9, 3) + 0), (substr(k.code, 12, 2) + 0)
from kladr k inner join short_names s on k.socr = s.scname"
                                    '(name short_id code gninmb uno ocatd status region_code distinct_code city_code town_code actuality_code)))
    (execute-non-query *db* (format nil "insert into kladr_objects(~{~a~^, ~})
select st.name, s.id, st.code, st.gninmb, st.uno, st.ocatd, (substr(st.code, 1, 2) + 0), (substr(st.code, 3, 3) + 0), (substr(st.code, 6, 3) + 0), (substr(st.code, 9, 3) + 0), (substr(st.code, 12, 4) + 0), (substr(st.code, 16, 2) + 0) from street st inner join short_names s on st.socr = s.scname"
                                    '(name short_id code gninmb uno ocatd region_code distinct_code city_code town_code street_code actuality_code)))))


(defun kladr-create-short-names ()
  (let ((table-fields '((id integer primary key not null)
                        (scname text not null)
                        (name text not null))))
    (sqlite::create-table *db* 'short_names table-fields '("unique(scname)" "unique(name)"))
    (execute-non-query *db* "insert into short_names(scname, name) select distinct k.socr, s.socrname from kladr k inner join socrbase s on k.socr = s.scname where not exists(select sn.* from short_names sn where sn.scname = k.socr)")
    (execute-non-query *db* "insert into short_names(scname, name) select distinct st.socr, s.socrname from street st inner join socrbase s on st.socr = s.scname where not exists(select sn.* from short_names sn where sn.scname = st.socr)")
    (values)))

(defmacro kladr-with-transaction (&body body)
  `(with-transaction *db*
     ,@body))

(defun kladr-make-hierarchy (&optional filter)
  (sqlite::create-table *db* 'kladr_hierarchy '((id integer primary key not null)
                                                (parent integer not null)
                                                (child integer not null))
                        '("unique(child)"
                          "foreign key (parent) references kladr_objects(id)"
                          "foreign key (child) references kladr_objects(id)"))
  (execute-non-query *db*
                     "create trigger if not exists clear_hieararchy before insert on kladr_hierarchy for each row begin
delete from kladr_hierarchy where child = new.child;
end")
  (let ((fields '(region_code distinct_code city_code town_code street_code actuality_code))
        (defaults '((= 0) (= 0) (= 0) (= 0) (is null) (= 0))))
    (
    
  
                                                