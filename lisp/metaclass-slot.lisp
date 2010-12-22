

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

(defmethod closer-mop:direct-slot-definition-class ((class new-class) &key overburn)
  (if overburn
      'new-direct-slot-class
      (call-next-method)))

(defmethod closer-mop:effective-slot-definition-class ((class new-class) &rest keys)
  (declare (ignore keys))
  'new-effective-slot-class)

(defmethod closer-mop:validate-superclass ((class new-class) (super standard-class))
  t)

(defclass aa ()
  ((a :initarg :a :initform 100)
   (b :initarg :b :initform 200 :overburn 100500))
  (:metaclass new-class))


