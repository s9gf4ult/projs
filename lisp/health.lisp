

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
                                         
                                             
                                        
                                        