(require 'closer-mop)

(defclass logging-class (closer-mop:standard-class)
  ((logging :initarg :logging
            :initform t
            :reader logging-class-logging)
   (logging-function :initarg :logging-function
                     :initform #'(lambda (slot old-value &optional (new-value nil new-value?))
                                   (let ((name (closer-mop:slot-definition-name slot)))
                                     (write-line (if new-value?
                                                     (format nil "slot ~a was ~a and now ~a" name old-value new-value)
                                                     (format nil "slot ~a has value ~a" name old-value)))))
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

(defmethod closer-mop:slot-value-using-class :around ((class logging-class) instance (eslot lc-effective-slot-definition))
  (let ((ret (call-next-method)))
    (when (and (logging-class-logging class) (lc-effective-slot-definition-read-logging eslot) (functionp (logging-class-logging-function class)))
      (funcall (logging-class-logging-function class) eslot ret))
    ret))

(defmethod (setf closer-mop:slot-value-using-class) :around (val (class logging-class) instance (eslot lc-effective-slot-definition))
  (call-next-method)
  (when (and (logging-class-logging class) (lc-effective-slot-definition-write-logging eslot) (functionp (logging-class-logging-function class)))
    (funcall (logging-class-logging-function class) eslot nil val)))
                                                       
(defclass new ()
  ((a :write-logging t
      :read-logging t
      :initform 10
      :initarg :a
      :accessor new-a))
  (:metaclass logging-class))

(setf (slot-value (find-class 'new) 'logging-function) #'(lambda (slot old-val &optional new-val)
                                    (write-line (format nil "<<<:::::==- ~a -- ~a -- ~a -==:::::>>>" (closer-mop:slot-definition-name slot) old-val new-val))))
