(defun write-lines (lines &key (filename #P"/home/razor/tmp/out.txt"))
  "записывает список строк в файл"
  (with-open-file (fout filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (loop for line in lines do
         (write-line line fout))))

(defun makefields (first last setable &key agregator (filename #P"/home/razor/tmp/out.txt"))
  "делает запрос по сетаблу с агрегированием или без, записывает в файл"
  (with-open-file (fout filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format fout "select idmu, rowno, ~a from setable~a where sdate >= ? and edate <= ? and idmu = ? ~a"
            (format nil "~{~a~^, ~}" 
              (loop for val from first to last
               collect (apply #'format (append
                                        (list nil
                                              (if agregator
                                                  (format nil "~a(field_~~a) as field_~~a" agregator)
                                                  "field_~a"))
                                        (if agregator
                                            (list val val)
                                            (list val))))))
              setable
              (if agregator
                  "group by idmu, rowno"
                  ""))))
                                         
(defun reset-flags (sdate edate guid org-ids)
  "выдает строку с запросом на удаление флажков для периода sdate edate и таблицы guid для органицаций с org-ids"
  (format nil "delete from se$states_d where state = 2 and sdate = '~a' and edate = '~a' and objguid = '~a' and idmu in (~{~a~^, ~});"
          sdate edate guid
          org-ids))

(defun reset-multiple-flags (sdate edate guids org-ids)
  "выдает список строк на удаление флажков с множества таблиц для множества организаций"
  (loop for guid in guids collect
       (reset-flags sdate edate guid org-ids)))
                                        
                                        