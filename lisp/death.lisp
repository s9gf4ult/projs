(defun get-headers (path)
  "забирает заголовки из выгрузки смертности в список"
  (with-open-file (fin path :direction :input)
    (ppcre:split ";" (read-line fin))))


(defun get-data (path &optional (skip-first t))
  "забирает данные из выгрузки `skip-first' пропускает первую строку, возвращает список списков. подсписки являются записями"
  (with-open-file (fin path)
    (when skip-first (read-line fin nil))
    (loop for a = (read-line fin nil) while a collect (ppcre:split ";" a :limit (length a)))))


(defun get-the-same (head1 head2)
  "возвращает список списков номеров элементов которые друг другу `equal' в каждом подсписке два элемента - номер в первом списке и номер во втором"
  (let ((eq-pares nil))
    (dotimes (a (length head1))
      (dotimes (b (length head2))
        (when (equal (elt head1 a) (elt head2 b))
          (push (list a b) eq-pares))))
    eq-pares))

(defun get-data-from-pares (pares data1 data2)
  "возвращает список списков. каждый подсписок состоит из двух элементов из `data1' и `data2' соответвенно, номера элементов берутся из `pares' (первый и второй элемернты подсписка). далее в возвращаемом списке идут элементы подсписка из `pares'"
  (mapcar #'(lambda (a b)
              (mapcar #'(lambda (pare)
                          (append (list (elt a (first pare)) (elt b (second pare))) pare )) pares)) data1 data2))
      

(defun convert-death-base (path-in)
  "принимает путь к файлу со старой базой, возвращает список списков с переконверченой базой"
  (let ((left->rigth (with-open-file (fin #P"/home/razor/tmp/death/left-to-right.lisp")
                       (read fin)))
        (right-base (make-hash-table :test #'equal))
        (left-data (get-data path-in))
        (new-len 110)
        (crnmb 0))
    (dolist (lft left->rigth)
      (setf (gethash (second lft) right-base) (first lft)))
    (labels (
             (get-number ()
               (setf crnmb (+ 1 crnmb))
               crnmb)
             (rec-construct (head tail accum data-list)
               (cond
                 ((>= accum new-len) head)
                 (t (progn
                      (setf (car tail) (let ((elt-nmb (gethash accum right-base)))
                                         (if elt-nmb
                                             (elt data-list elt-nmb)
                                             ""))
                            (cdr tail) (if (< accum (- new-len 1))
                                           (cons nil nil)
                                           nil))
                      (rec-construct head (cdr tail) (+ 1 accum) data-list)))))
             )
      (mapcar #'(lambda (a)
                  (let* ((new-list (cons nil nil))
                         (new-data (rec-construct new-list new-list 0 a)))
                    (setf (first new-data) (get-number))
                    new-data)) left-data))))


(defun convert-and-write-database (path1 path2)
  "коневертим дазу банных, просто указываем пути входной и выходной"
  (with-open-file (fout path2 :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (loop for a in (convert-death-base path1) do (write-line (format nil "~{~a~^;~}" a) fout))))
                            
                                              
                                         
(defun get-max-difs (flist)
  "возвращает список разниц межжу соседними элементами в списке (не помню зачем такое мне было нужно)"
  (labels ((rec-get (ret-head ret-tail head tail)
             (cond
               ((not (cdr tail)) ret-head)
               (t (let ((dif (abs (- (car tail) (car head)))))
                    (cond
                      ((member dif ret-head) (rec-get ret-head ret-tail (cdr head) (cdr tail)))
                      (t (progn
                           (setf (car ret-tail) dif
                                 (cdr ret-tail) (cons nil nil))
                           (rec-get ret-head (cdr ret-tail) (cdr head) (cdr tail))))))))))
    (let ((new-list (cons nil nil)))
      (rec-get new-list new-list  flist (cdr flist)))))

(defun magis-six (flist)
  "принимает список с базой в новом формате и заменяет в нем встреченные поля H1 равные 6 на 8"
  (let ((h1number 34)) ;; magic number where sits many six
    (mapcar #'(lambda (a)
                (if (ignore-errors (eql (parse-integer (elt a h1number)) 6))
                    (let ((newa (copy-list a)))
                      (setf (elt newa h1number) 8)
                      newa)
                    a)) flist)))

(defun save-data (flist path)
  "записывает данные из списка в файл"
  (with-open-file (fout path :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (loop for a in flist do (write-line (format nil "~{~a~^;~}" a) fout))))
