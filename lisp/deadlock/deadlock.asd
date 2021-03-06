(asdf:defsystem deadlock
  :depends-on (:cl-ppcre :sqlite :alexandria :iterate :closer-mop :lift)
  :components (
               (:file "packages")
               (:file "common" :depends-on ("packages"))
               (:file "conditions" :depends-on ("packages" "common"))
               ;; (:file "classes" :depends-on ("conditions"))
               ;; (:file "generics" :depends-on ("classes"))
               ;; (:file "methods" :depends-on ("generics"))
               (:file "collection" :depends-on ("packages"))
               (:file "tests" :depends-on ("collection"))
               ))