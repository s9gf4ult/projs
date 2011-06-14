(in-package :kladr)

(defun open-kladr-database (filename)
  (connect (list filename) :database-type :sqlite3 :if-exists :new)
  (enable-sql-reader-syntax))

(defun close-kladr-database ()
  (disable-sql-reader-syntax)
  (disconnect))

(defun make-kladr-objects ()
  (with-transaction ()
    (when (member "kladr_objects" (list-tables))
      (drop-table kladr_objects))
    (let ((kfields '(([id] integer :primary-key :not-null)
                     ([name] text :not-null)
                     ([socrid] integer :not-null)
                     ([code] text :not-null :unique)
                     ([index] integer)
                     ([gninmb] text)
                     ([uno] text)
                     ([ocatd] integer)
                     ([status] integer)
                     ([region_code] integer :not-null)
                     ([distinct_code] integer :not-null)
                     ([city_code] integer :not-null)
                     ([locality_code] integer :not-null)
                     ([street_code] integer)
                     ([actuality_code] integer :not-null))))
    (create-table kladr_objects
                  kfields
                  :constraints '("unique(region_code, distinct_code, city_code, locality_code, street_code, actuality_code)"
                                 "foreign key (socrid) references short_names(id)"))
    (insert-records :into kladr_objects
                    :attributes (mapcar #'car (cdr kfields))
                    :query "select k.name, s.id, k.code, k.index, k.gninmb, k.uno, k.ocatd, k.status, to_number(substr(k.code, 1, 2)), substr(k.code, 3, 3), 
                     