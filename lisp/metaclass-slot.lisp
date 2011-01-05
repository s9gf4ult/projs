(require 'closer-mop)

(defclass new-direct-slot-class (closer-mop:standard-direct-slot-definition)
  ((overburn :initarg :overburn
             :initform nil
             :accessor new-slot-class-overburn)))

(defclass new-effective-slot-class (closer-mop:standard-effective-slot-definition)
  ((overburn :initarg :overburn
             :initform nil
             :accessor new-effective-slot-class-overburn)))

(defclass new-class (closer-mop:standard-class)
  ())

(defclass new-root-class ()
  ()
  (:metaclass new-class))

(defmethod closer-mop:compute-class-precedence-list :around ((class new-class))
  (let ((ret (call-next-method)))
    (if (member-if #'(lambda (a) (typep a 'new-class)) ret)
        ret
        (cons (find-class 'new-root-class) ret))))

(defmethod closer-mop:direct-slot-definition-class :around ((class new-class) &key overburn)
  (if overburn
      (find-class 'new-direct-slot-class)
      (call-next-method)))

;; (defmethod closer-mop:effective-slot-definition-class ((class new-class) &rest keys)
;;   (declare (ignore keys))
;;   (find-class 'new-effective-slot-class))

(defmethod closer-mop:compute-effective-slot-definition :around ((class new-class) name dslots)
  (if (find-if #'(lambda (a) (typep a 'new-direct-slot-class)) dslots)
      (find-class 'new-effective-slot-class)
      (call-next-method)))
      

(defmethod closer-mop:validate-superclass ((class new-class) (super standard-class))
  t)

(defmethod closer-mop:slot-value-using-class ((class new-class) instance (slotd new-effective-slot-class))
  (declare (ignore instance class))
  (* (new-effective-slot-class-overburn slotd) (call-next-method)))

(defmethod closer-mop:slot-definition-allocation ((slotd new-effective-slot-class))
  (declare (ignore slotd))
  :instance)

(defclass aa ()
  ((a :initarg :a :initform 100)
   (b :initarg :b :initform 200 :overburn 100500))
  (:metaclass new-class))


