(defun get-headers (path)
  (with-open-file (fin path :direction :input)
    (ppcre:split ";" (read-line fin))))


(defun get-data (path &optional (skip-first t))
  (with-open-file (fin path)
    (when skip-first (read-line fin nil))
    (loop for a = (read-line fin nil) while a collect (ppcre:split ";" a :limit (length a)))))


(defun get-the-same (head1 head2)
  (let ((eq-pares nil))
    (dotimes (a (length head1))
      (dotimes (b (length head2))
        (when (equal (elt head1 a) (elt head2 b))
          (push (list a b) eq-pares))))
    eq-pares))

(defun get-data-from-pares (pares data1 data2)
  (mapcar #'(lambda (a b)
              (mapcar #'(lambda (pare)
                          (append (list (elt a (first pare)) (elt b (second pare))) pare )) pares)) data1 data2))
      

(defun convert-death-base (path-in)
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
  (with-open-file (fout path2 :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (loop for a in (convert-death-base path1) do (write-line (format nil "~{~a~^;~}" a) fout))))
                            
                                              
                                         
(defun get-max-difs (flist)
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
