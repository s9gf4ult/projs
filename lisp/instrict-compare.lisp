;; (defun make-standard-weights (&key (letter-letter 1) (double-space 0.1) (space-letter-space 1.2) (double-letter 0.5) (quotes-other-qouotes 0.1) (double-number 2) (number-number 2) (number-other-number 2) (other 0.1))
;;   (let* ((weights (make-hash-table :test #'equal))
;;          (alpha "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя")
;;          (numbers "1234567890")
;;          (numbers-len (length numbers))
;;          (other-chars ",.<>/?;:'\\][}{!@#$%^&*()_+")
;;          (other-chars-len (length other-chars))
;;          (alpha-len (length alpha))
;;          (all-syms (concatenate 'string alpha numbers other-chars " \""))
;;          (all-syms-len (+ other-chars-len numbers-len alpha-len)))
    
;;     (dotimes (a alpha-len)
;;       (dotimes (b alpha-len)
;;         (when (not (eql a b))
;;           (setf (gethash (cons (elt alpha a) (elt alpha b)) weights) letter-letter))))
;;     (setf (gethash (cons #\Space #\Space) weights) double-space)
;;     (dotimes (a alpha-len)
;;       (let ((celt (elt alpha a)))
;;         (setf (gethash (cons #\Space celt) weights) space-letter-space
;;               (gethash (cons celt #\Space) weights) space-letter-space
;;               (gethash (cons celt celt) weights) double-letter)))
;;     (dotimes (a all-syms-len)
;;       (let ((celt (elt all-syms a)))
;;         (setf (gethash (cons #\" celt) weights) quotes-other-qouotes
;;               (gethash (cons celt #\") weights) quotes-other-qouotes)))
;;     (dotimes (a numbers-len)
;;       (dotimes (b numbers-len)
;;         (setf (gethash (cons (elt numbers a) (elt numbers b)) weights) (if (eql a b)
;;                                                                            double-number
;;                                                                            number-number))))
;;     (let*((not-numbers (concatenate 'string alpha other-chars " \""))
;;           (not-nu-len (length not-numbers)))
;;       (dotimes (b numbers-len)
;;         (let ((cnelt (elt numbers b)))
;;           (dotimes (a not-nu-len)
;;             (let ((celt (elt not-numbers a)))
;;               (setf (gethash (cons cnelt celt) weights) number-other-number
;;                     (gethash (cons celt cnelt) weights) number-other-number))))))
;;     (setf (gethash :other weights) other)

;;     weights))

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
              (let ((cweight (length (lcs-list (string->list (string-downcase lelt)) (string->list (string-downcase (car relt)))))))
              ;(let ((cweight (instrict-compare (string-downcase lelt) (string-downcase (car relt)))))
                (when (or (not found) (> cweight (car found)))
                  (setf found (cons cweight relt))))))
        (when found
          (push (cons lelt (cdr found)) ret))))
    ret))



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

                  