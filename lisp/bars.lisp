(defun write-lines (lines &optional stream)
  "Пишет список строк в файл разделяя строки переводом коретки
stream должен быть потоком"
  (let* ((closef nil)
         (stream (or stream
                     (progn
                       (setf closef t)
                       (open "/home/razor/tmp/get.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)))))
    (loop for line in lines do
         (write-line line stream))
    (when closef (close stream))))

(defun reduce-string (reductor strings)
  "склеивает список строк в одну, между строками вставляет reductor"
  (when strings
    (reduce (lambda (a b)
              (format nil "~a~a~a" a reductor b)) strings)))

(defun gen-rows (start stop column)
  "генерит список строк вида $столбец:строка$"
  (loop for c from start to stop collect
       (format nil "$~a:~a$" column c)))

(defun gen-cell-addreses (table-name col-start col-end row-start row-end)
  "генерит список строк вида таблица:столбец:строка для квадратной области в таблице с именем table-name"
  (loop for row from row-start to row-end append
       (loop for col from col-start to col-end collect
            (format nil "~a:~a:~a" table-name col row))))

(defun reduce-strings-1000 (reductor strings &optional (size 1000))
  "тоже что и reduce-string только еще дополнительно разбивает результат на строки
в каждой не более 1000 символов возвращает сисок"
  (let (ret
        tmp
        (tmplen 0))
    (loop for st in strings do
         (progn
           (incf tmplen (+ (length st) (length reductor)))
           (if (> tmplen size)
               (progn
                 (push (reverse tmp) ret)
                 (setf tmp (list st)
                       tmplen (+ (length st) (length reductor))))
               (push st tmp)))
       :finally (push tmp ret))
    (reverse (loop for r in ret collect
                  (reduce-string reductor r)))))
         