
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

(macrolet ((defcondcomb (name operator)
             (let ((formater (format nil "(~~a ~a ~~a)" operator))
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
      `(qif ,(caar pairs)
            (,(cadar pairs))
             ((qcond ,@(cdr pairs))))))

(qcond ((qand "a = 10" "b = 100")
        "a = a * b"
        "c = c + 1"
        )
       ((qor "c = 100")
        "out = out + 100"))
             
            

(defmacro acol (&rest lsls)
  (if lsls
      `(,(car lsls)
         ,@(cdr lsls))))

     


                                        ;(defun qcond (&rest pairs)
  
           
    
    
                  

(defun indent-all (strs)
  (mapcar #'(lambda (a)
              (format nil "  ~a" a)) strs))