(defpackage :lispy-qpile
  (:use 'cl))

(in-package 'lispy-qpile)

(defun qpile-for (val from to strings)
  (declare (type list strings)
           (type number from to)
           (string val))
  (append
   (list (format nil "FOR ~a FROM ~a TO ~a" val from to))
   (indent-all strings)
   (list (format nil "END FOR"))))

(defun qpile-for-in (val instr strings)
  (declare (type list strings)
           (type string val)
           (type (or list string) instr))
  (let ((for-str (if (typep instr 'list)
                     (format nil "FOR ~a IN \"~{~a~^,~}\"" val instr)
                     (format nil "FOR ~a IN ~a" val instr))))
    (append
     (list for-str)
     (indent-all strings)
     (list "END FOR"))))

(defun qpile-func (name args-list strings)
  (declare (type list args-list strings)
           (type string name))
  (append
   (list (format nil "FUNC ~a (~{~a~^, ~})" name args-list))
   (indent-all strings)
   (list "END FUNC")))

(defmacro qfunc (name-and-args &body body)
  (let ((name (format nil "~a" (car name-and-args)))
        (args (mapcar #'(lambda (a)
                          (format nil "~a" a))
                      (cdr name-and-args)))
        (super-body (mapcar #'(lambda (a)
                                (if (typep a 'list)
                                    a
                                    `(list ,a))) body)))
    `(qpile-func ,name (list ,@args)
                 (append ,@super-body))))

(defmacro qfor (var-and-other &body body)
  (let ((super-body (mapcar #'(lambda (a)
                                (if (typep a 'list) a
                                    `(list ,a))) body))
        (var (format nil "~a" (car var-and-other))))
    (if (eq (second var-and-other) 'in)
        `(qpile-for-in ,var (list ,@(cddr var-and-other))
                       (append ,@super-body))
        `(qpile-for ,var ,(cadr var-and-other) ,(caddr var-and-other)
                    (append ,@super-body)))))

(defun qpile-if (condition stringsif &optional (stringselse nil))
  (append
   (list (format nil "IF ~a" condition))
   (indent-all stringsif)
   (when stringselse
     (append
      (list "ELSE")
      (indent-all stringselse)))
   (list "END IF")))

(defmacro qif (condition bodyif &optional bodyelse)
  (labels ((procbody (body)
             (mapcar #'(lambda (a)
                         (if (typep a 'list)
                             a
                             `(list ,a))) body)))
    (let ((sbodyif (procbody bodyif))
          (sbodyelse (procbody bodyelse)))
      `(qpile-if ,condition
                 (append ,@sbodyif)
                 (append ,@sbodyelse)))))

(macrolet ((defqcomb (name operator)
             (let ((formater (format nil "(~~{~~a~~^ ~a ~~})" operator)))
               `(defun ,name (&rest args)
                  (format nil ,formater args))))
           (defall (&rest pares)
             `(progn
                ,@(loop for a in pares collect `(defqcomb ,(car a) ,(cadr a))))))
  (defall
      (qand and)
      (qor or)
    (qmul *)
    (qadd +)
    (qdiv /)
    (qsub -)
    (qconcat &)))

(defun qstr (val)
  (format nil "\"~a\"" val))

(defmacro qstrl (&rest vals)
  `(,@(loop for val in vals collect (format nil "\"~a\"" val))))

(macrolet ((defcondcomb (name operator)
             (let ((formater (format nil "~~a ~a ~~a" operator))
                   (and-formater "(~{~a~^ AND ~})"))
               `(defun ,name (&rest args)
                  (format nil ,and-formater
                          (reduce #'append (maplist #'(lambda (a)
                                                        (if (cdr a)
                                                            (list (format nil ,formater (car a) (cadr a)))
                                                            nil)) args))))))
           (defall (&rest pares)
             `(progn
                ,@(loop for a in pares collect `(defcondcomb ,(car a) ,(cadr a))))))
  (defall
      (qeq =)
      (qmore >)
    (qless <)
    (qmoreq >=)
    (qlesseq <=)
    (qneq !=)))



(defmacro qcond (&rest pairs)
  (if pairs
      (if (and (cdr pairs)
               (eq 't (caadr pairs)))
          `(qif ,(caar pairs)
                (,@(cdar pairs))
                (,@(cdadr pairs)))
          
          `(qif ,(caar pairs)
                (,@(cdar pairs))
                ((qcond ,@(cdr pairs)))))))


(defun qpile-while (condition strings)
  (let ((smsm (gensym)))
    (append
     (list (format nil "FOR ~a FROM 1 TO 2" smsm))
     (indent-all (append
                  (list (format nil "~a = 1" smsm))
                  (qpile-if condition
                            strings
                            '("BREAK"))))
     '("END FOR"))))

(defun qpile-until (condition strings)
  (let ((smsm (gensym)))
    (append
     (list (format nil "FOR ~a FROM 1 TO 2" smsm))
     (indent-all (append
                  (list (format nil "~a=1" smsm))
                  strings
                  (qpile-if condition
                            '("BREAK"))))
     '("END FOR"))))

(defmacro qwhile (condition &body body)
  (let ((sbody (mapcar #'(lambda (a)
                           (if (typep a 'list)
                               a
                               `(list ,a))) body)))
    `(qpile-while ,condition
                  (append
                   ,@sbody))))

(defmacro quntil (condition &body body)
  (let ((sbody (mapcar #'(lambda (a)
                           (if (typep a 'list)
                               a
                               `(list ,a))) body)))
    `(qpile-until ,condition
                  (append
                   ,@sbody))))
                  

(defun indent-all (strs)
  (mapcar #'(lambda (a)
              (format nil "  ~a" a)) strs))

(defun qpile-map-values (variable &rest pairs)
  (loop for pair in pairs collect (format nil "~a = SET_VALUE(~a, ~a, ~a)" variable variable (car pair) (cadr pair))))

(defmacro qforms (&body body)
  (let ((sbody (mapcar #'(lambda (a)
                           (if (typep a 'list)
                               a
                               `(list ,a))) body)))
    `(append
      ,@sbody)))

(defun write-on (filename strings)
  (with-open-file (fout filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (loop for a in strings do (write-line a fout))))

(defun qset (name val)
  (list (format nil "~a = ~a" name val)))

(defun qglobal (name &optional val)
  (list (if val (format nil "NEW_GLOBAL(~a, ~a)" name val)
            (format nil "NEW_GLOBAL(~a)" name))))

(defun qgetval (dest src name)
  (format nil "GET_VALUE(~a, ~a)" src name))

(defun qpile-apply (name &rest args)
  (format nil "~a(~{~a~^, ~})" name args))

                  
(defun create-collection-of-strings(name &rest values)
  (append
   (list (format nil "~a = CREATE_COLLECTION()" name))
   (loop for val in values collect (format nil "~a = INSERT_COLLECTION_ITEM(~a, \"~a\")" name name val))))

(defun create-date-values (name list-of-dates)
  (append
   (list (format nil "~a = CREATE_COLLECTION()" name))
   (let ((nmb -1))
     (loop for years in list-of-dates append
          (loop for months in (cdr years) append
               (loop for day in (car (cdr months)) collect (progn
                                                       (setf nmb (+ nmb 1))
                                                       (format nil "~a = INSERT_COLLECTION_ITEM(~a, ~a, \"~a\"" name name nmb
                                                               (format nil "~4,'0d~2,'0d~2,'0d" (car years) (car months) day)))))))))










(progn
  (defparameter *qpile-dispatchers* (make-hash-table))
  (macrolet ((setmany (&rest pairs)
               `(setf ,@(loop for pair in pairs append
                             `((gethash ',(car pair) *qpile-dispatchers*) ',(cadr pair))))))
    (setmany (for for-path)
             (defun defun-path))))
                                                        
(defclass hash-stacks ()
  ((stacks :accessor stacks :initform (make-hash-table :test #'equal) :initarg :stacks :type hash-table))
  (:documentation "stores stack of values for any key"))

(defgeneric hash-stacks-push (place key obj))

(defmethod hash-stacks-push ((place hash-stacks) key obj)
  (multiple-value-bind (val isit?) (gethash key (stacks place))
    (setf (gethash key (stacks place))
          (if isit?
              (cons obj val)
              (list obj)))))

(defgeneric hash-stacks-pop (place key))

(defmethod hash-stacks-pop ((place hash-stacks) key)  
  (multiple-value-bind (val isit?) (gethash key (stacks place))
    (if isit?
        (multiple-value-prog1
            (values (car val) t)
          (if (cdr val)
              (setf (gethash key (stacks place)) (cdr val))
              (remhash key (stacks place))))
        (values nil nil))))

(defgeneric hash-stacks-top (place key))
(defmethod hash-stacks-top ((place hash-stacks) key)
  (multiple-value-bind (val isit?) (gethash key (stacks place))
    (if isit?
        (values (car val) t)
        (values nil nil))))

(defclass transformed ()
  ((execution :type list :initform nil :initarg :execution :accessor transformed-execution)
   (returned :initform nil :initarg :returned :accessor transformed-returned)))

(define-condition qpile-error (error)  ())

(define-condition qpile-bad-form (qpile-error)
  ((form-value :initarg :form-value :reader form-value)))

(define-condition qpile-incorrect-dispatcher (qpile-bad-form)
  ())

(defun with-pushed-vars (env vars func)
  (dolist (var vars)
    (hash-stacks-push env var (gensym)))
  (prog1
      (funcall func)
    (dolist (var vars)
      (hash-stacks-pop env var))))

(defun process-form-in-environment (env form)
  (restart-case
      (cond
        ((listp form) (dispatch-form env form))
        ((stringp form) (list form))
        ((numberp form) (list (format nil "~a" form)))
        (t (error (make-condition 'qpile-bad-form :form-value form))))
    (return-nil () nil)))

(defun dispatch-form (env form)
  (multiple-value-bind (disp isit?) (gethash (car form) *qpile-dispatchers*)
    (if (not isit?) (error (make-condition 'qpile-incorrect-dispatcher :form-value form)))
    (funcall disp env (cdr form))))


(defun lispy-qpile (&rest forms)
  (let ((env (make-instance 'hash-stacks)))
    (labels ((process-one-form (form)
               (process-form-in-environment env form)))
      (reduce #'append (mapcar #'process-one-form forms)))))


(defun generate ()
  (lispy-qpile
   `(defun func1 (arg1 arg2)
      (let ((a (+ arg1 arg2))
            (b (* arg1 arg2)))
        (/ a b)))
   ))