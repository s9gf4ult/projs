
(define-condition get-root-error (error)
  ((desk :initarg :desk
         :accessor desk
         :initform nil)
   (cap :initarg :cap
        :initform nil
        :accessor cap)))

(defclass initial-conditions ()
  ((args :initarg :args
         :accessor args
         :initform nil)))

(defgeneric get-desk (init))

(defmethod get-desk ((init initial-conditions))
  (let ((a (first (args init)))
        (b (second (args init)))
        (c (third (args init))))
    (- (* b b)
       (* 4 a c))))

(defgeneric get-root (init nmb))

(defmethod get-root ((init initial-conditions) (nmb integer))
  (if (not (<= 0 nmb 1))
      (restart-case (error (make-condition 'get-root-error :cap "number must be 0 or 1"))
        (skip-eval () nil)
        (get-narrow () (let ((nnmb (if (< nmb 0)
                                       0 1)))
                         (get-root init nnmb))))
      (let ((desk (get-desk init)))
        (flet ((calculate-roots  (init desk nmb)
                 (let ((deskq (sqrt desk))
                       (op (cond ((= nmb 0) '+)
                                 (t '-)))
                       (a (first (args init)))
                       (b (second (args init))))
                   (/ (funcall op (- 0 b) deskq)
                      (* 2 a)))))
        
          (if (< desk 0)
              (restart-case (error (make-condition 'get-root-error :cap "desk is less then 0" :desk desk))
                (skip-eval () nil)
                (reverse-a () (let ((new-init (make-instance 'initial-conditions :ARGS (list (- 0 (first (args init)))
                                                                                             (second (args init))
                                                                                             (third (args init))))))
                                (get-root new-init nmb)))
                (zerro-c () (let ((new-init (make-instance 'initial-conditions :args (list (first (args init))
                                                                                           (second (args init))
                                                                                           0))))
                              (get-root new-init nmb)))
                (reverse-desk () (let ((new-desk (- 0 desk)))
                                   (calculate-roots (init new-desk nmb)))))
              (calculate-roots (init desk nmb)))))))
              
              

                  
                         


  
  
          

         