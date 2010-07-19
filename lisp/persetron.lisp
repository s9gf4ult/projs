(defclass neiron ()
  ((synlist :initform nil
            :initarg :synlist
            :accessor synlist)
   (accumulator :initform 0
                :accessor accumulator)
   (reductor :initform #'(lambda (a b)
                           (+ a b))
             :initarg :reductor
             :accessor reductor
             :documentation "function that reduces incoming signal and accumulated value
lambda list: (accumulator signal)
return value: new value of an accumulator")))

(defclass synaps ()
  ((weight :initform #'(lambda (sig)
                         sig)
           :initarg :weight
           :accessor weight
           :documentation "weight function
Lambda list:(signal)
signal - signal object
return value: result signal to add to neiron")
   (transfered :initform nil
               :accessor transfered)
   (neiron :initform nil
           :initarg :neiron
           :accessor neiron)))

(defgeneric connect (synaps neiron)
  (:documentation "connects synaps to neiron"))

(defmethod connect ((synaps synaps) (neiron neiron))
  (setf (neiron synaps) neiron))

(defgeneric transfer-signal (synaps signal)
  (:documentation "transfers signal to neiron of synaps"))

(defmethod transfer-signal ((synaps synaps) signal)
  (if (neiron synaps)
      (if (not (transfered synaps))
          (let* ((neiron (neiron synaps))
                 (accumulator (accumulator neiron)))
            (setf (accumulator neiron)
                  (funcall (reductor neiron) accumulator (funcall (weight synaps) signal)))
            (setf (transfered synaps) t))
          nil)
      nil))

(defgeneric connect-neirons (neiron1 neiron2 synaps)
  (:documentation "connects two neirons by synaps"))

(defmethod connect-neirons ((neiron1 neiron) (neiron2 neiron) (synaps synaps))
  (push synaps (synlist neiron1))
  (connect synaps neiron2))

(defclass mulator-synaps (synaps)
  ((mulator :initform 1
            :initarg :mulator
            :accessor mulator)
   (weight :initform nil)))

(defmethod shared-initialize :after ((obj mulator-synaps)  slot-names &rest initargs &key key)
  (setf (weight obj) #'(lambda (x)
                         (* (mulator obj) x))))


          
                  
  

    