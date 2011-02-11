(defun write-lines (lines &optional stream)
  (let* ((closef nil)
         (stream (or stream
                     (progn
                       (setf closef t)
                       (open "/home/razor/tmp/get.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)))))
    (loop for line in lines do
         (write-line line stream))
    (when closef (close stream))))

(defun gen-rows (start stop column)
  (loop for c from start to stop collect
       (format nil "$~a:~a$" column c)))
                       