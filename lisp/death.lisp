(defun get-headers (path)
  (with-open-file (fin path :direction :input)
    (ppcre:split ";" (read-line fin))))


(defun get-data (path)
  (with-open-file (fin path)
    (read-line fin nil)
    (loop for a = (read-line fin nil) while a collect (ppcre:split ";" a :limit (length a)))))


