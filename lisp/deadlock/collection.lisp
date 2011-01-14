(in-package :deadlock)

(defclass collection ()
  ((objects :initform (make-hash-table :test #'eql)
            :initarg :objects
            :reader collection-objects)
   (property-getter :initform #'class-of
                    :initarg :property-getter
                    :reader collection-property-getter)
   (property-inheriter :initform #'closer-mop:subclassp
                       :initarg :property-inheriter
                       :reader collection-property-inheriter)))

(defmethod add-object ((col collection) obj)
  (let* ((c (funcall (collection-property-getter col) obj))
         (objs (gethash c (collection-objects col))))
    (setf (gethash c (collection-objects col)) (delete-duplicates (cons obj objs)))))

(defmethod delete-object ((col collection) obj)
  (let* ((c (funcall (collection-property-getter col) obj))
         (objs (gethash c (collection-objects col)))
         (res (delete obj objs)))
    (if res 
        (setf (gethash c (collection-objects col)) res)
        (remhash c (collection-objects col)))))

(defmethod find-objects ((col collection) property)
  (let ((res nil))
    (maphash #'(lambda (k v)
                 (when (funcall (collection-property-inheriter col) k property)
                   (setf res (append res v)))) (collection-objects col))
    res))

(defmethod find-and-delete-objects ((col collection) property)
  (maphash-keys #'(lambda (k)
                    (when (funcall (collection-property-inheriter col) k property)
                      (remhash k (collection-objects col)))) (collection-objects col)))
         
         