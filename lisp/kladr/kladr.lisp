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
                (kassign id cid)))       ;привязываем улицы к городам
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

(defun kladr-shrink-root-level()
  (execute-non-query *db* "delete from kladr_hierarchy where parent in (select k.id from kladr_objects k where exists(select h.* from kladr_hierarchy h where h.parent = k.id) and not exists(select hh.* from kladr_hierarchy hh where hh.child = k.id))"))

(defmacro within-multithread (&body body)
  `(within-main-loop-and-wait
     (gdk:gdk-threads-enter)
     (prog1
         (progn
           ,@body)
       (gdk:gdk-threads-leave))))
          

(defun draw-hierarchy-tree ()
  "Draw tree of kladr objects"
  (with-main-loop
    (let-ui (gtk-window :title "Kladr tree"
                        :height-request 400
                        :width-request 600
                        :position :center
                        :var window
                        (scrolled-window
                         (tree-view :var view)))
      (let ((store (make-instance 'tree-store :column-types '("gint" "gchararray" "gchararray")))
            threads)
        (flet ((build-root ()
                 (iter (for (id tp nm) in-sqlite-query "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id where exists(select h.* from kladr_hierarchy h where h.parent = k.id) and not exists(select hh.* from kladr_hierarchy hh where hh.child = k.id) order by t.name, k.name" on-database *db*)
                       (for x from 0)
                       (for pr = (within-multithread
                                   (tree-store-insert-with-values store nil x id tp nm)))

                            ;; (within-main-loop-and-wait
                            ;;        (gdk:gdk-threads-enter)
                            ;;        (prog1
                            ;;            (tree-store-insert-with-values store nil x id tp nm)
                            ;;          (gdk:gdk-threads-leave))))
                                   
                       (iter (for (cid ctp cnm) in-sqlite-query "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name" on-database *db* with-parameters (id))
                             (for y from 0)
                             (within-multithread
                               (tree-store-insert-with-values store pr y cid ctp cnm))))))
                             ;; (within-main-loop-and-wait
                             ;;   (gdk:gdk-threads-enter)
                             ;;   (prog1
                             ;;       (tree-store-insert-with-values store pr y cid ctp cnm)
                             ;;     (gdk:gdk-threads-leave)))))
          (push (bordeaux-threads:make-thread #'build-root) threads))
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
                                  (named-lambda row-expanded-handler (tree it path)
                                    (declare (ignore tree path))
                                    (let ((cid (tree-model-value store it 0)))
                                      (unless (member cid expanded)
                                        (let ((child (tree-model-iter-first-child store it)))
                                          (when child
                                            (flet ((build-child ()
                                                     (iter
                                                       (iter (for (cid ctp cnm) in-sqlite-query "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name" on-database *db* with-parameters ((within-main-loop-and-wait (tree-model-value store child 0))))
                                                             (for pos from 0)
                                                             (within-multithread
                                                               (tree-store-insert-with-values store child pos cid ctp cnm)))
                                                       (while
                                                           (within-multithread
                                                             (tree-model-iter-next store child))))
                                                     ))
                                              ;(build-child))))
                                              (push (bordeaux-threads:make-thread #'build-child) threads))))
                                        (push cid expanded))))))
        (gobject:connect-signal window "delete-event"
                                #'(lambda (widget event)
                                    (declare (ignore widget event))
                                    (iter (for thread in threads)
                                          (if (bordeaux-threads:thread-alive-p thread)
                                              (bordeaux-threads:destroy-thread thread)))
                                    (gtk-main-quit)
                                    nil))
        )
       
      (widget-show window))))


(defun draw-hierarchy-tree-monothread ()
  "Draw tree of kladr objects"
  (with-main-loop
    (let-ui (gtk-window :title "Kladr tree"
                        :height-request 400
                        :width-request 600
                        :position :center
                        :var window
                        (scrolled-window
                         (tree-view :var view)))
      (let ((store (make-instance 'tree-store :column-types '("gint" "gchararray" "gchararray"))))
        (iter (for (id tp nm) in-sqlite-query "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id where exists(select h.* from kladr_hierarchy h where h.parent = k.id) and not exists(select hh.* from kladr_hierarchy hh where hh.child = k.id) order by t.name, k.name" on-database *db*)
              (for x from 0)
              (for pr = (tree-store-insert-with-values store nil x id tp nm))
              (iter (for (cid ctp cnm) in-sqlite-query "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name" on-database *db* with-parameters (id))
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
                                              (iter (for (cid ctp cnm) in-sqlite-query "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name" on-database *db* with-parameters ((tree-model-value store child 0)))
                                                    (for pos from 0)
                                                    (tree-store-insert-with-values store child pos cid ctp cnm))
                                              (while
                                                  (tree-model-iter-next store child)))))
                                        (push cid expanded))))))
        )
       
      (widget-show window))))