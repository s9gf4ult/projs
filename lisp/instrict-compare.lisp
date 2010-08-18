(defun make-standard-weights (&key (letter-letter 1) (double-letter 0.2) (number-number 5) (number-other 2.5) (space-not-quote 0.5) (double-space 0.05) (quote-other 0.1) (double-quote 0.1) (other 0.1))
  (lambda (pare)
    (declare (type cons pare))
    (let ((alp "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыььэюя")
          (numb "1234567890")
          (a (car pare))
          (b (cdr pare)))
      
      (cond
        ((and (position a alp)
              (position b alp)) (if (eql a b)
                                    double-letter
                                    letter-letter))
        ((and (position a numb)
              (position b numb)) number-number)
        ((and (eql #\Space a)
              (eql #\Space b)) double-space)
        ((and (eql #\" a)
              (eql #\" b)) double-quote)
        ((or (position a numb)
             (position b numb)) number-other)
        ((or (eql #\" a)
             (eql #\" b)) quote-other)
        ((or (eql a #\Space)
             (eql b #\Space)) space-not-quote)
        (t other)))))
            
    
(defun instrict-compare (a b &key (weights (make-standard-weights)) (mono-weight 0.1) (test #'eql))
  "compares two sequences instrictly, returns weight number calculated according weight function `weight' which must get `cons' and return weight of similarity, this weight will be mulated to count of similarityes. Additionally `mono-weight' is the weight if single-charahter similarity"
  (declare (type sequence a b)
           (type function weights test))
  (macrolet ((hash-add (table key)
               (let ((val (gensym))
                     (valp (gensym)))
                 `(multiple-value-bind (,val ,valp) (gethash ,key ,table)
                    (setf (gethash ,key ,table) (if ,valp
                                                    (+ 1 ,val)
                                                    1))))))
    (labels (
             (get-seqs (st)
               (declare (type sequence st))
               (when (> (length st) 0)
                 (let ((seq-hash (make-hash-table :test #'equal))
                       (mono-hash (make-hash-table :test #'equal)))
                   (dotimes (el (- (length st) 1))
                     (let ((fst (elt st el))
                           (scn (elt st (+ 1 el))))
                       (hash-add mono-hash fst)
                       (hash-add seq-hash (cons fst scn))))
                   (let ((lvl (elt st (- (length st) 1))))
                     (hash-add mono-hash lvl))
                   (values mono-hash seq-hash))))
             
             (get-weight (h1 h2)
               (declare (type hash-table h1 h2))
               (let ((found (make-hash-table :test #'equal))
                     (accum 0))
                 (maphash #'(lambda (key1 val1)
                              (multiple-value-bind (val2 val2p) (gethash key1 h2)
                                (if val2p
                                    (setf (gethash key1 found) t
                                          accum (+ accum (* (local-weight key1)
                                                            (- (min val1 val2)
                                                               (abs (- val1 val2))))))
                                    (setf accum (- accum (* (local-weight key1)
                                                            val1)))))) h1)
                 (maphash #'(lambda (key2 val2)
                              (when (not (gethash key2 found))
                                (setf accum (- accum (* (local-weight key2)
                                                        val2))))) h2)
                 accum))

             (local-weight (key)
               (declare (type (or cons character) key))
               (cond
                 ((characterp key) mono-weight)
                 ((consp key) (funcall weights key)))))
                                                     
                                      
      (multiple-value-bind (a-mono a-seq) (get-seqs a)
        (multiple-value-bind (b-mono b-seq) (get-seqs b)
          (+ (get-weight a-mono b-mono)
             (get-weight a-seq b-seq)))))))
            

(defun get-much-similary (str str-seq)
  (reduce #'(lambda (a b)
              (cond
                ((> (second a) (second b)) a)
                ((< (second a) (second b)) b)
                (t a))) (mapcar #'(lambda (a)
                                    (list a (instrict-compare str a))) str-seq )))

(defun produce-lists-comparation (left right)
  "`left' must be list of strings, `right' is the list of lists which first element is string. Returns list if lists which first element is `left' and tails is `right' which fist element more similary (but not equal) with `left'"
  (declare (type list left right))
  (let ((ret nil))
    (dolist (lelt left)
      (let ((found nil))
        (dolist (relt right)
          (if (string= lelt (car relt))
              (progn (setf found nil)
                     (return))
              (let ((cweight (compare-by-words (string-downcase lelt) (string-downcase (car relt)))))
                (when (or (not found) (> cweight (car found)))
                  (setf found (cons cweight relt))))))
        (when found
          (push (cons lelt (cdr found)) ret))))
    ret))

(defun produce-lists-comparation-old (left right)
  "`left' must be list of strings, `right' is the list of lists which first element is string. Returns list if lists which first element is `left' and tails is `right' which fist element more similary (but not equal) with `left'"
  (declare (type list left right))
  (let ((ret nil))
    (dolist (lelt left)
      (let ((found nil))
        (dolist (relt right)
          (if (string= lelt (car relt))
              (progn (setf found nil)
                     (return))
              (let ((cweight (instrict-compare (string-downcase lelt) (string-downcase (car relt)))))
                (when (or (not found) (> cweight (car found)))
                  (setf found (cons cweight relt))))))
        (when found
          (push (cons lelt (cdr found)) ret))))
    ret))


(defun produce-maybe-equal (string-list arg-list)
  (declare (type list string-list arg-list))
  (let ((ret nil))
    (dolist (str1 string-list ret)
      (let ((found-equal (loop for a in arg-list when (strings-maybe-equal str1 (car a)) collect a)))
        (when found-equal
          (push (cons str1 found-equal) ret))))))
          


;; (defun max-lcs (seq1 seq2)
;;   (declare (type array seq1 seq2))
;;   (let ((l1 (length seq1))
;;         (l2 (length seq2)))
;;     (let* ((aseq (if (> l1 l2)
;;                      seq2
;;                      seq1))
;;            (bseq (if (> l1 l2)
;;                      seq1
;;                      seq2))
;;            (alen (length aseq))
;;            (blen (length bseq))
;;            (amas (make-array '(alen) :adjustable nil :element-type 'fixnum :initial-element 0))
;;            (bmas (make-array '(blen) :adjustable nil :element-type 'fixnum :initial-element 0)))
      
            


(defun lcs-list (list-1 list-2 &key (test #'eql))
  "Find the longest common subsequence of LIST-1 and LIST-2 using TEST."
  (cond
    ((null list-1) nil)
    ((null list-2) nil)
    ((funcall test (first list-1) (first list-2))
       (cons (first list-1) (lcs-list (rest list-1) (rest list-2) :test test)))
    (t (let ((lcs-1 (lcs-list list-1 (rest list-2) :test test))
             (lcs-2 (lcs-list (rest list-1) list-2 :test test)))
         (if (> (length lcs-1) (length lcs-2))
           lcs-1
           lcs-2)))))

(defun string->list (str)
  (let* ((ret (cons nil nil))
         (tail ret)
         (len (length str)))
    (dotimes (a len)
      (setf (car tail) (elt str a))
      (when (< a (- len 1))
            (setf (cdr tail) (cons nil nil)
                  tail (cdr tail))))
    ret))
 
(defun diff (list1 list2 &key (test #'eql))
  "Find the differences between LIST1 and LIST2 using TEST."
  (let ((lcs (lcs-list list1 list2 :test test))
        result)
    (dolist (c lcs)
      (let* ((sync-list1 (position c list1 :test test))
             (sync-list2 (position c list2 :test test))
             (removed (subseq list1 0 sync-list1))
             (added (subseq list2 0 sync-list2)))
        (setf list1 (subseq list1 (1+ sync-list1)))
        (setf list2 (subseq list2 (1+ sync-list2)))
        (when removed
          (push (cons :removed removed) result))
        (when added
          (push (cons :added added) result))
        (push c result)))
    (when list1
      (push (cons :removed list1) result))
    (when list2
      (push (cons :added list2) result))
    (nreverse result)))


(defun split-by-words (str)
  (declare (type string str))
  (delete-if #'(lambda (a) (and (<= (length a) 3)
                                (not (ppcre:scan "[0-9]" a)))) (ppcre:split "(-|\\(|\\)|\\.|\\ |\"|,)+" str)))

(defun compare-coefficient (str1 str2)
  (/ (instrict-compare str1 str2)
     (instrict-compare str1 str1)))

(defun strings-maybe-equal (str1 str2)
  (> (/ (compare-by-words str1 str2)
        (compare-by-words str1 str1)) 0.5))

(defun compare-coefficient-by-words (str1 str2)
  (/ (compare-by-words str1 str2)
     (compare-by-words str1 str1)))

(defun compare-by-words (str1 str2)
  (declare (type string str1 str2))
  (let ((ww1 (split-by-words str1))
        (ww2 (split-by-words str2))
        (acc 0))
    (if (or (= 0 (length ww1)) (= 0 (length ww2)))
        0
        (dolist (w1 ww1 acc)
          (cond
            ((= 0 (length ww2)) (return))
            (t (let* ((weights (mapcar #'(lambda (a) (list (compare-coefficient w1 a) a )) ww2))
                      (maxelt (reduce #'(lambda (a b)
                                          (if (> (car a) (car b))
                                              a
                                              b)) weights)))
                 (when (> (car maxelt) 0)(setf acc (+ acc (car maxelt)))))))))))
                      
                 
                            

(defun get-mutum (path)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((delete-double-quotes (str)
             (ppcre:regex-replace-all "\"\"" str "\"")))
    (let ((inc nil)
          (ret nil))
      (with-open-file (fin path :direction :input)
        (loop for a = (read-line fin nil) while a do (push a inc)))
      (dolist (el inc ret)
        (multiple-value-bind (val vals) (ppcre:scan-to-strings "^([0-9]+)\\s+\"(.*)\"$" el)
          (when (> (length vals) 1)
            (push (list (delete-double-quotes (aref vals 1))
                        (aref vals 0)) ret)))))))
  
            
(defun get-strings-from-file (path)
  (with-open-file (fin path :direction :input)
    (loop for a = (read-line fin nil) while a collect a)))

(defun manual-filter-list (flist)
  (remove-if #'(lambda (a)
                 (write-line (format nil "~a" (first a)))
                 (write-line (format nil "~a" (second a)))
                 (and (string/= (first a) (second a))
                      (string/= "y" (string-downcase (read-line))))) flist))

(defun produce-list-equation (strs lists)
  (declare (type list strs lists))
  (let ((ret nil))
    (dolist (st strs ret)
      (let ((much-more (reduce #'(lambda (a b)
                                   (if (> (car a) (car b))
                                       a
                                       b)) (mapcar #'(lambda (a)
                                                       (cons (compare-by-words st (car a))
                                                             a)) lists))))
        (push (cons st (cdr much-more)) ret)))))

(defun uniq (flist)
  (labels ((rec-rem (head tail)
             (cond
               ((not (cdr tail)) head)
               ((equal (car tail) (cadr tail)) (progn
                                                 (setf (car tail) (cadr tail)
                                                       (cdr tail) (cddr tail))
                                                 (rec-rem head  tail)))
               (t (rec-rem head (cdr tail))))))
    (rec-rem flist flist)))
                 
    