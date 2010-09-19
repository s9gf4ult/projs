
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
                                                               
                                                        
(defmacro xchg (&rest expand-all)
  (let ((exp (mapcar #'eval expand-all)))
    `(list ,@(reverse (mapcar #'(lambda (a) (car a)) exp)))))
  
(defmacro lispy-qpile (&rest forms)
  (let ((processed (mapcar #'(lambda (form)
                               (labels ((expand-form (local-form)
                                          (case (car local-form)
                                            (':string (mapcar #'(lambda (str)
                                                                      (format nil "~a" str)) (cdr local-form))))))
                                 (expand-form form))) forms)))
    `(list
      ,@(reduce #'append processed))))

