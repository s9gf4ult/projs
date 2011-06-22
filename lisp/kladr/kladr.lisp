(in-package :kladr)

(defvar *db* nil)
(defvar *spec-id* nil)

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
    (sqlite::create-table *db* 'kladr_objects table-fields '(
                                                             ;"unique(region_code, distinct_code, city_code, town_code, street_code, actuality_code)"
                                                              "unique(code)"
                                                              "foreign key (short_id) references short_names(id)"))
    (execute-non-query *db* (format nil "insert into kladr_objects(~{~a~^, ~})
select k.name, s.id, k.code, k.gninmb, k.uno, k.ocatd, k.status, (substr(k.code, 1, 2) + 0), (substr(k.code, 3, 3) + 0), (substr(k.code, 6, 3) + 0), (substr(k.code, 9, 3) + 0), (substr(k.code, 12, 2) + 0)
from kladr k inner join short_names s on k.socr = s.scname"
                                    '(name short_id code gninmb uno ocatd status region_code distinct_code city_code town_code actuality_code)))
    (execute-non-query *db* (format nil "insert into kladr_objects(~{~a~^, ~})
select st.name, s.id, st.code, st.gninmb, st.uno, st.ocatd, (substr(st.code, 1, 2) + 0), (substr(st.code, 3, 3) + 0), (substr(st.code, 6, 3) + 0), (substr(st.code, 9, 3) + 0), (substr(st.code, 12, 4) + 0), (substr(st.code, 16, 2) + 0) from street st inner join short_names s on st.socr = s.scname"
                                    '(name short_id code gninmb uno ocatd region_code distinct_code city_code town_code street_code actuality_code)))))

(defun kladr-drop-all-created ()
  (execute-non-query *db* "drop table kladr_hierarchy")
  (execute-non-query *db* "drop table kladr_objects")
  (execute-non-query *db* "drop table short_names"))


(defun kladr-create-short-names ()
  (let ((table-fields '((id integer primary key not null)
                        (scname text not null)
                        (name text not null))))
    (sqlite::create-table *db* 'short_names table-fields '("unique(scname)" "unique(name)"))
    (execute-non-query *db* "insert into short_names(scname, name) select distinct k.socr, s.socrname from kladr k inner join socrbase s on k.socr = s.scname where not exists(select sn.* from short_names sn where sn.scname = k.socr)")
    (execute-non-query *db* "insert into short_names(scname, name) select distinct st.socr, s.socrname from street st inner join socrbase s on st.socr = s.scname where not exists(select sn.* from short_names sn where sn.scname = st.socr)")
    (execute-non-query *db* "insert into short_names(scname, name) values (?, ?)" "Группа.Об." "Группа объектов")
    (setf *spec-id* (last-insert-rowid *db*))
    (values)))

(defmacro kladr-with-transaction (&body body)
  `(with-transaction *db*
     ,@body))

(defun execute-insert (tablename field-value)
  (let* ((names (mapcar #'car field-value))
         (quest (mapcar #'(lambda (a)
                            (declare (ignore a)) "?") names))
         (values (mapcar #'second field-value))
         (query (format nil "insert into ~a(~{~a~^, ~}) values (~{~a~^, ~})" tablename names quest)))
    (apply  #'execute-non-query `(,*db*
                                  ,query
                                  ,@values))))
                            

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
  (flet ((andfilter (str)
           (if filter
               (format nil "~a and (~a)" str filter)
               str)))
    (iter (for (id rcode) in-sqlite-query (andfilter "select id, region_code from kladr_objects where region_code <> 0 and distinct_code = 0 and city_code = 0 and town_code = 0 and street_code is null and actuality_code = 0") on-database *db*) ;итерируем по областям
          (iter (for (cid) in-sqlite-query (andfilter "select id from kladr_objects where region_code = ? and distinct_code <> 0 and city_code = 0 and town_code = 0 and street_code is null and actuality_code = 0") on-database *db* with-parameters (rcode))
                (kassign id cid))    ;привязываем регионы к областям
          (iter (for (cid) in-sqlite-query (andfilter "select id from kladr_objects where region_code = ? and distinct_code = 0 and city_code <> 0 and town_code = 0 and street_code is null and actuality_code = 0") on-database *db* with-parameters (rcode))
                (kassign id cid)))       ;привязываем города обласного значения к областям
          
    (iter (for (id rcode dcode) in-sqlite-query (andfilter "select id, region_code, distinct_code from kladr_objects where region_code <> 0 and distinct_code <> 0 and city_code = 0 and town_code = 0 and street_code is null and actuality_code = 0") on-database *db*) ;итерируем по регионам
          (iter (for (cid) in-sqlite-query (andfilter "select id from kladr_objects where region_code = ? and distinct_code = ? and city_code <> 0 and town_code = 0 and street_code is null and actuality_code = 0 order by name") on-database *db* with-parameters (rcode dcode))
                (kassign id cid))      ;привязываем года к регионам
          (iter (for (cid) in-sqlite-query (andfilter "select id from kladr_objects where region_code = ? and distinct_code = ? and city_code = 0 and town_code <> 0 and street_code is null and actuality_code = 0") on-database *db* with-parameters (rcode dcode))
                (kassign id cid)))       ;привязываем населенные пункты к регионам
    (iter (for (id rcode dcode ccode) in-sqlite-query (andfilter "select id, region_code, distinct_code, city_code from kladr_objects where region_code <> 0 and city_code <> 0 and town_code = 0 and street_code is null and actuality_code = 0") on-database *db*) ;итерируем по городам
          (iter (for (cid) in-sqlite-query (andfilter "select id from kladr_objects where region_code = ? and distinct_code = ? and city_code = ? and town_code <> 0 and street_code is null and actuality_code = 0") on-database *db* with-parameters (rcode dcode ccode))
                (kassign id cid))       ;привязываем населенные пункты к городам (должны быть всякие территории и микраши)
          (iter (for (cid) in-sqlite-query "select id from kladr_objects where region_code = ? and distinct_code = ? and city_code = ? and town_code = 0 and street_code <> 0 and actuality_code = 0" on-database *db* with-parameters (rcode dcode ccode))
                (kassign id cid))       ;привязываем улицы к городам

          (when (> (execute-one-row-m-v *db* "select count(k.id) from kladr_objects k inner join kladr_hierarchy h on k.id = h.child where h.parent = ? and k.street_code is null" id) 0)
            (let ((cid (progn
                         (execute-insert "kladr_objects" `(("name" "Населенные пункты")
                                                           ("short_id" ,*spec-id*)
                                                           ("code" ,(uuid:print-bytes nil (uuid:make-v1-uuid)))
                                                           ("region_code" ,rcode)
                                                           ("distinct_code" ,dcode)
                                                           ("city_code" ,ccode)
                                                           ("town_code" 0)
                                                           ("street_code" -1)
                                                           ("actuality_code" 0)))
                         (last-insert-rowid *db*))))
              (execute-non-query *db* "update kladr_hierarchy set parent = ? where id in (select h.id from kladr_hierarchy h inner join kladr_objects k on k.id = h.child where h.parent = ? and k.street_code is null)" cid id)
              (kassign id cid)))

          (when (> (execute-one-row-m-v *db* "select count(k.id) from kladr_objects k inner join kladr_hierarchy h on k.id = h.child where h.parent = ? and k.street_code is not null" id) 0)
            (let ((cid (progn
                         (execute-insert "kladr_objects" `(("name" "Улицы переулки и пр.")
                                                           ("short_id" ,*spec-id*)
                                                           ("code" ,(uuid:print-bytes nil (uuid:make-v1-uuid)))
                                                           ("region_code" ,rcode)
                                                           ("distinct_code" ,dcode)
                                                           ("city_code" ,ccode)
                                                           ("town_code" 0)
                                                           ("street_code" -1)
                                                           ("actuality_code" 0)))
                         (last-insert-rowid *db*))))
              (execute-non-query *db* "update kladr_hierarchy set parent = ? where id in (select h.id from kladr_hierarchy h inner join kladr_objects k on k.id = h.child where h.parent = ? and k.street_code is not null and k.street_code <> -1)" cid id)
              (kassign id cid))))
                            
            
          ;; (iter (for (tid tname) in-sqlite-query (andfilter "select distinct t.id, t.name from short_names t inner join kladr_objects k1 on k1.short_id = t.id inner join kladr_hierarchy h on h.child = k1.id where h.parent = ?") on-database *db* with-parameters (id))
          ;;       (let* ((nns '(name short_id code region_code distinct_code city_code town_code street_code actuality_code))
          ;;              (chid (progn
          ;;                      (execute-non-query *db* (format nil "insert into kladr_objects(~{~a~^, ~}) values (~{~a~^, ~})" nns (mapcar #'(lambda (a)
          ;;                                                                                                                                      (declare (ignore a)) "?") nns)) tname *spec-id* (print-bytes nil (make-v1-uuid)) rcode dcode ccode 0 -1 0)
          ;;                      (last-insert-rowid *db*))))
          ;;         (kassign id chid)
          ;;         (execute-non-query *db* "update kladr_hierarchy set parent = ? where child in (select k1.id from kladr_objects k1 inner join kladr_hierarchy h on h.child = k1.id where h.parent = ? and k1.short_id = ?)" chid id tid))))
                
    (iter (for (id rcode dcode ccode tcode) in-sqlite-query (andfilter "select id, region_code, distinct_code, city_code, town_code from kladr_objects where region_code <> 0 and town_code <> 0 and street_code is null and actuality_code = 0") on-database *db*) ;итерируем по населенным пунктам
          (iter (for (cid) in-sqlite-query "select id from kladr_objects where region_code = ? and distinct_code = ? and city_code = ? and town_code = ? and street_code <> 0 and actuality_code = 0" on-database *db* with-parameters (rcode dcode ccode tcode))
               (kassign id cid)))       ;привязываем улицы к населенным пунктам
          ))

   


(defun kladr-make-me-happy()
  (handler-bind
      ((simple-error #'(lambda (e)
                             (let ((r (find-restart 'drop-table)))
                               (when r (invoke-restart r))))))
    (kladr-create-objects)
    (kladr-make-hierarchy)))
    

(defun kassign (parent-id child-id)
  (execute-non-query *db* "insert into kladr_hierarchy(parent, child) values (?, ?)" parent-id child-id))

(defun kladr-shrink-root-level (&optional (table-name "kladr_hierarchy"))
  (execute-non-query *db* (format nil "delete from ~a~:* where parent in (select k.id from kladr_objects k where exists(select h.* from ~a~:* h where h.parent = k.id) and not exists(select hh.* from ~a hh where hh.child = k.id))" table-name)))


(defmacro within-multithread (&body body)
  `(within-main-loop-and-wait
     (gdk:gdk-threads-enter)
     (prog1
         (progn
           ,@body)
       (gdk:gdk-threads-leave))))
          
(defun draw-hierarchy-tree (&optional (table-name "kladr_hierarchy"))
  "Draw tree of kladr objects"
  (with-main-loop
    (let-ui (gtk-window :title "Kladr tree"
                        :height-request 400
                        :width-request 600
                        :position :center
                        :var window
                       (v-box 
                        (scrolled-window
                         (tree-view :var view))
                        (h-box
                         (button :label "Save to file" :var save-button) :expand nil
                         ) :expand nil))
      
                         
      (flet ((from-table (query)
               (format nil query table-name)))
        (let ((store (make-instance 'tree-store :column-types '("gint" "gchararray" "gchararray"))))
          (iter (for (id tp nm) in-sqlite-query (from-table "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id where exists(select h.* from ~a~:* h where h.parent = k.id) and not exists(select hh.* from ~a hh where hh.child = k.id) order by t.name, k.name") on-database *db*)
                (for x from 0)
                (for pr = (tree-store-insert-with-values store nil x id tp nm))
                (iter (for (cid ctp cnm) in-sqlite-query (from-table "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join ~a h on h.child = k.id where h.parent = ? order by t.name, k.name") on-database *db* with-parameters (id))
                      (for y from 0)
                      (tree-store-insert-with-values store pr y cid ctp cnm)))
          (setf (tree-view-model view) store)
          (let ((column (make-instance 'tree-view-column :title "Type"))
                (renderer (make-instance 'cell-renderer-text)))
            (tree-view-column-pack-start column renderer)
            (tree-view-column-add-attribute column renderer "text" 1)
            (tree-view-append-column view column))
          (let ((column (make-instance 'tree-view-column :title "Name"))
                (renderer (make-instance 'cell-renderer-text)))
            (tree-view-column-pack-start column renderer)
            (tree-view-column-add-attribute column renderer "text" 2)
            (tree-view-append-column view column))
          (gobject:connect-signal view "row-expanded"
                                  (let (expanded)
                                    #'(lambda (tree it path)
                                        (declare (ignore tree path))
                                        (let ((cid (tree-model-value store it 0)))
                                          (unless (member cid expanded)
                                            (let ((child (tree-model-iter-first-child store it)))
                                              (when child
                                                (iter
                                                  (iter (for (cid ctp cnm) in-sqlite-query (from-table "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join ~a h on h.child = k.id where h.parent = ? order by t.name, k.name") on-database *db* with-parameters ((tree-model-value store child 0)))
                                                        (for pos from 0)
                                                        (tree-store-insert-with-values store child pos cid ctp cnm))
                                                  (while
                                                      (tree-model-iter-next store child)))))
                                            (push cid expanded))))))
          (gobject:g-signal-connect save-button "clicked"
                                    #'(lambda (button)
                                        (declare (ignore button))
                                        (let ((file-name (gtk-query-file-name-to-save)))
                                          (when file-name
                                            (save-to-xml file-name)))))
          ))
      
      (widget-show window))))

(defun gtk-query-file-name-to-save ()
  (let ((dialog (make-instance 'file-chooser-dialog :action :save :title "Save to XML file")))
    (dialog-add-button dialog "gtk-cancel" :cancel)
    (dialog-add-button dialog "gtk-save" :ok)
    (let ((resp (dialog-run dialog)))
      (unwind-protect
           (when (equal resp :ok)
             (file-chooser-filename dialog))
        (object-destroy dialog)))))

(defun make-element(name &optional attributes)
  (let ((ret (xtree:make-element name)))
    (iter (for (name value) in attributes)
          (setf (xtree:attribute-value ret name) value))
    ret))


(defun save-to-xml (filename &optional (pretty-print t))
  (let* ((doc (xtree:make-document))
         (root (xtree:append-child doc (make-element "УниверсальныйСправочник" '(("Группа" "Учреждения")
                                                                                 ("Код" "Кладр")
                                                                                 ("Наименование" "Кладр")
                                                                                 ("Иерархический" "Да")
                                                                                 ("РежимВыбораЗаписей" "Все")))))
         (desc (xtree:append-child root (make-element "Описание")))
         (actl (xtree:append-child root (make-element "ПериодДействия" '(("Начало" "")
                                                                         ("Конец" "31.12.9999 0:00:00")))))
         (assign (xtree:append-child root (make-element "ПривязкаУчреждений")))
         (attrs (xtree:append-child root (make-element "Атрибуты")))
         (records (xtree:append-child root (make-element "Записи"))))
    (iter (for (id) in-sqlite-query "select k.id from kladr_objects k where exists (select h.* from kladr_hierarchy h where h.parent = k.id) and not exists (select hh.* from kladr_hierarchy hh where hh.child = k.id)" on-database *db*)
          (xtree:append-child records (make-element-tree id)))
    (with-open-file (fout filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      (xtree:serialize doc fout :pretty-print pretty-print))))

(defun make-element-tree (element-id)
  (multiple-value-bind (code name)
      (execute-one-row-m-v *db* "select k.name, t.name from kladr_objects k inner join short_names t on k.short_id = t.id where k.id = ?" element-id)
    (let ((ret (make-element "Запись" `(("Код" ,code)
                                        ("Наименование" ,(format nil "~a ~a" name code))))))
      (iter (for (id) in-sqlite-query "select k.id from kladr_objects k inner join kladr_hierarchy h on k.id = h.child where h.parent = ?" on-database *db* with-parameters (element-id))
            (xtree:append-child ret (make-element-tree id)))
      ret)))

(defun calculate-ununique-names-count()
  (execute-one-row-m-v *db* "select count(name) from (select n.name as name, count(k.id) as count from (select distinct name from kladr_objects) n inner join kladr_objects k on k.name = n.name group by n.name) where count > 1"))

