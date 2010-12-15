; а вот и первый метакласс для подсчета выделенных инстансов
(require 'trivial-garbage)
(require 'closer-mop)

(defclass counting-class (standard-class)
  ((count :initform 0 :initarg :count :accessor counting-class-count)))
(defmethod counting-class-count ((cls symbol))
  (counting-class-count (find-class cls)))
(defmethod (setf counting-class-count) (val (cls symbol))
  (setf (counting-class-count (find-class cls)) val))
(defmethod (setf counting-class-count) :before ((val number) (cls counting-class))
  (when (< val 0)
    (warn "class ~a has count = ~a" cls val)))

(defmethod make-instance :around ((class counting-class) &rest all-keys)
  (incf (counting-class-count class))
  (let ((obj (if all-keys (call-next-method class all-keys) (call-next-method class))))
    (tg:finalize obj #'(lambda () (decf (counting-class-count class))))))

(defmethod closer-mop:validate-superclass ((class counting-class) (super standard-class))
  t)

(defmethod closer-mop:validate-superclass ((class standard-class) (super counting-class))
  nil)

(defclass sample-counter ()
  ()
  (:metaclass counting-class))
(defun fout (string &rest all-other)
  (write-line (if all-other (format nil string all-other) (format nil string))))

(defun test-counter ()
  (fout "There is ~a instances now" (counting-class-count 'sample-counter))
  (let ((a (loop for a from 1 to 1000000 collect (make-instance 'sample-counter))))
    (fout "We are in the let form, we have a list of ~a instances" (length a))
    (fout "Class talk us that there is ~a instances" (counting-class-count 'sample-counter)))
  (fout "We did exit from let form, there is ~a instances" (counting-class-count 'sample-counter))
  (tg:gc)
  (fout "After GC there is ~a instances" (counting-class-count 'sample-counter)))
  



