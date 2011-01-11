(require 'closer-mop)

(defclass logging-class (closer-mop:standard-class)
  ((logging :initarg :logging
            :initform t
            :reader logging-class-logging)
   (logging-function :initarg :logging-function
                     :initform #'print
                     :accessor logging-class-logging-function)))

(defclass lc-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((read-logging :initarg :read-logging
                 :reader lc-direct-slot-definition-read-logging)
   (write-logging :initarg :write-logging
                  :reader lc-direct-slot-definition-write-logging)))

(defclass lc-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((read-logging :initform nil
                 :initarg :read-logging
                 :accessor lc-effective-slot-definition-read-logging)
   (write-logging :initform nil
                  :initarg :write-logging
                  :accessor lc-effective-slot-definition-write-logging)))

(defmethod closer-mop:validate-superclass ((class logging-class) (super standard-class))
  t)

(defmethod closer-mop:direct-slot-definition-class ((class logging-class) &rest initargs)
  (declare (ignore class initargs))
  (find-class 'lc-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class logging-class) &rest initargs)
  (declare (ignore class initargs))
  (find-class 'lc-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class logging-class) slot-name dslots)
  (let ((slot (call-next-method)))
    (setf (lc-effective-slot-definition-read-logging slot) (let ((fst (find-if #'(lambda (s) (and (typep s 'lc-direct-slot-definition) (slot-boundp s 'read-logging))) dslots)))
                                                        (when fst
                                                          (lc-direct-slot-definition-read-logging fst))))
    (setf (lc-effective-slot-definition-write-logging slot) (let ((fst (find-if #'(lambda (s) (and (typep s 'lc-direct-slot-definition) (slot-boundp s 'write-logging))) dslots)))
                                                              (when fst
                                                                (lc-direct-slot-definition-write-logging fst))))
    slot))

(defmethod closer-mop:slot-value-using-class ((class logging-class) instance (eslot lc-effective-slot-definition))
  (let ((ret (call-next-method)))
    (when (and (logging-class-logging class) (lc-effective-slot-definition-read-logging eslot) (functionp (logging-class-logging-function class)))
      (funcall (logging-class-logging-function class) ret))
    ret))

                                                       
(defclass new ()
  ((a :write-logging t
      :read-logging t
      :initform 10
      :initarg :a
      :accessor new-a))
  (:metaclass logging-class
   :logging nil
  :logging-function #'(lambda (mes) (write-line (format nil "that was accessed ~a value" mes)))))
              
